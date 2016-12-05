{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Awful
import React.Flux.Components
import React.Flux.Components.Util

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.DeepSeq (NFData)
import Control.Exception (SomeException (..), displayException)
import Control.Monad (void)
import Data.Aeson (decode')
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Function (on)
import Data.Maybe (fromMaybe, isNothing, isJust, listToMaybe)
import Data.Metrology.Computing (Byte (Byte), Core (Core), (%>))
import Data.Metrology.SI (Second (Second), mega, centi)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Eclogues.API (API, JobError, JobOutput, OutputType (Stdout), displayError)
import qualified Eclogues.Job as Job
import GHC.Generics (Generic)
import Lens.Micro (Lens', (^.), (.~), (&), lens)
import Lens.Micro.TH (makeLenses)
import qualified Network.HTTP.Types.Status as HTTP
import Network.URI (URI (..), URIAuth (..), uriToString)
import Path (parseAbsFile)
import Servant.API ((:<|>) (..), NoContent)
import Servant.Client (BaseUrl (..), ServantError, Scheme (Http), client)
import qualified Servant.Client as SC
import Servant.Utils.Links (safeLink)

newtype Status = Status { unStatus :: Job.Status }
               deriving (Show, Generic, NFData)

type StatusKey = Job.Name

statusKey :: Status -> StatusKey
statusKey = (^. Job.name) . unStatus

instance Eq Status where
    (==) = (==) `on` statusKey

instance Ord Status where
    compare = comparing statusKey

data State = State { page         :: !Page
                   , statuses     :: !(Set Status)
                   , refreshError :: !(Maybe SubmitError)
                   , pspec        :: !PartialSpec
                   , submitStatus :: !SubmitStatus
                   , deleteStatus :: !DeleteStatus
                   , listZoom     :: !ListZoom }

data Page = JobList
          | AddJob
          deriving (Eq, Generic, NFData)

data ListZoom = ListZoom { filterKey :: !(Maybe StatusKey)
                         , topKey    :: !(Maybe StatusKey) }
              deriving (Generic, NFData)

data ListView = ListView { prevSpan :: ![Status]
                         , curSpan  :: ![Status] }

data SubmitStatus = NotSubmitted
                  | Submitting
                  | SubmitFailure !SubmitError
                  deriving (Eq, Generic, NFData)

data DeleteType = Delete | Cancel
                deriving (Show, Eq, Generic, NFData)

data DeleteStatus = DeleteStatus !Job.Name !DeleteType !SubmitStatus
                  deriving (Eq, Generic, NFData)

data SubmitError = InvalidResponse !Text
                 | ConnectionError !Text
                 | FailureResponse !JobError
                 deriving (Show, Eq, Generic, NFData)

data PartialSpec = PartialSpec { _pname   :: !Text
                               , _pcmd    :: !Text
                               , _pram    :: !Word
                               , _pdisk   :: !Word
                               , _pcpu    :: !Word
                               , _ptime   :: !Word
                               , _ppaths  :: ![Text]
                               , _pstdout :: !Bool
                               , _pdeps   :: ![Text] }
                 deriving (Show, Generic, NFData)

$(makeLenses ''PartialSpec)

getJobs :: SC.ClientM [Job.Status]
putJobStage :: Job.Name -> Job.Stage -> SC.ClientM NoContent
deleteJob :: Job.Name -> SC.ClientM NoContent
createJob :: Job.Spec -> SC.ClientM NoContent
(     getJobs
 :<|> _
 :<|> _
 :<|> putJobStage
 :<|> _
 :<|> _
 :<|> deleteJob
 :<|> createJob
 :<|> _) = client (Proxy :: Proxy API)

clientEnv :: SC.ClientEnv
clientEnv = SC.ClientEnv $ BaseUrl Http Awful.hostname Awful.port ""

justError :: SC.ClientM NoContent -> IO (Maybe SubmitError)
justError = fmap (either (Just . convError) (const Nothing)) . (`SC.runClientM` clientEnv)

defPartialSpec :: PartialSpec
defPartialSpec = PartialSpec "" "" 10 10 1 60 [] False []

defListZoom :: ListZoom
defListZoom = ListZoom Nothing Nothing

listMax :: Word
listMax = 10

data Action = RefreshInit
            | RefreshReturn (Either SubmitError (Set Status))
            | SwitchPage Page
            | UpdateListZoom ListZoom
            | UpdatePSpec PartialSpec
            | SubmitJob Job.Spec
            | SubmitReturn (Maybe SubmitError)
            | DeleteJob Job.Name DeleteType
            | DeleteReturn Job.Name DeleteType (Maybe SubmitError)
            deriving (Generic, NFData)

instance StoreData State where
    type StoreAction State = Action
    transform RefreshInit        s      = updateJobs *> pure s
    transform (RefreshReturn r)  s      = do
        _ <- async $ threadDelay 1000000 *> alterStore store RefreshInit
        pure $ case r of
            Right ss'   -> s{ statuses = ss', refreshError = Nothing }
            Left  err   -> s{ refreshError = Just err }
    transform (SwitchPage np) s         = pure $ s{ page = np }
    transform (UpdateListZoom lz) s     = pure $ s{ listZoom = lz }
    transform (UpdatePSpec p) s         = pure $ s{ pspec = p }
    transform (SubmitJob j)   s         = do
        _ <- async $ alterStore store . SubmitReturn =<< justError (createJob j)
        pure $ s{ submitStatus = Submitting }
    transform (SubmitReturn Nothing)    s = pure s{ submitStatus = NotSubmitted
                                                  , pspec = defPartialSpec
                                                  , page = JobList }
    transform (SubmitReturn (Just e))   s = pure s{ submitStatus = SubmitFailure e }
    transform (DeleteJob n t)           s = do
        let action = case t of
                Delete -> deleteJob n
                Cancel -> putJobStage n $ Job.Failed Job.UserKilled
        _ <- async $ alterStore store . DeleteReturn n t =<< justError action
        pure s{ deleteStatus = DeleteStatus n t Submitting }
    transform (DeleteReturn n t (Just e)) s = pure s{ deleteStatus = DeleteStatus n t $ SubmitFailure e }
    transform (DeleteReturn _ _ Nothing)  s = pure s{ deleteStatus = defDeleteStatus }

-- TODO: handle errors more gracefully
updateJobs :: IO ()
updateJobs = void . async $ SC.runClientM getJobs clientEnv >>= \r ->
    alterStore store . RefreshReturn $ bimap convError (Set.fromList . fmap Status) r

defName :: Job.Name
defName = fromMaybe (error "invalid defName") $ Job.mkName "-"

defDeleteStatus :: DeleteStatus
defDeleteStatus = DeleteStatus defName Delete NotSubmitted

store :: ReactStore State
store = mkStore $ State JobList Set.empty Nothing defPartialSpec NotSubmitted defDeleteStatus defListZoom

dispatchState :: Action -> [SomeStoreAction]
dispatchState a = [SomeStoreAction store a]

app :: ReactView ()
app = defineControllerView "eclogues app" store $ \s () ->
    pageContainer_ $ do
        appHeader_
        mapM_ (alert_ Danger . elemText . showError) $ refreshError s
        case deleteStatus s of
            DeleteStatus n t (SubmitFailure e) ->
              let intro = case t of
                      Delete -> "Error deleting "
                      Cancel -> "Error cancelling "
              in alert_ Danger . elemText $ intro <> Job.nameText n <> ": " <> showError e
            _                                -> pure ()
        links_ $ page s
        section_ [htmlId mainId] $ pageElement_ s

mainId :: JSString
mainId = "main"

pageElement_ :: State -> Element
pageElement_ State{..} = case page of
    JobList -> statusList_ statuses listZoom
    AddJob  -> addJob_ (isJust refreshError) submitStatus pspec

appHeader_ :: Element
appHeader_ = pageHeader_ "Eclogues Jobs"

links_ :: Page -> Element
links_ cur = tabs_ $ do
    tab "Job List" JobList
    tab "Add Job"  AddJob
  where
    tab :: JSString -> Page -> Element
    tab lbl dest = linkTab_ active [href "#", goto dest, ariaControls mainId] $ elemStr lbl
      where
        active = dest == cur
    goto :: Page -> Prop Link
    goto dest = onClick $ \_ _ -> dispatchState $ SwitchPage dest

statusList_ :: Set Status -> ListZoom -> Element
statusList_ ss lz = do
    pagination_ lz lv
    table_ $ do
        thead_ . tr_ $ th_ [style $ width "50%"] "Name" <> th_ "Stage" <> th_ "Stdout" <> th_ "Delete"
        tbody_ . mapM_ statusRow_ $ take (fromIntegral listMax) (curSpan lv)
  where
    lv = lzoom lz ss

pagination_ :: ListZoom -> ListView -> Element
pagination_ lz@ListZoom{..} ListView{..} = pager_ "Job list pages" [style topStyle] $ do
    but prev "previous" (arr "← " <> "Previous") $ isJust topKey
    li_ [style displayCell] $ filterBox_ lz
    but next "next" ("Next" <> arr " →") $ listMaxInt < length curSpan
  where
    topStyle = width "100%" <> displayTable
            <> marginX "auto" <> marginTop "2ex" <> marginLow "1ex"
    endStyle = displayCell <> width "20ex"
    next _ _ = updateTop $ statusKey <$> listToMaybe (drop listMaxInt curSpan)
    prev _ _ = if dropping <= 0
        then updateTop Nothing
        else updateTop $ statusKey <$> listToMaybe (drop dropping prevSpan)
      where
        dropping = length prevSpan - listMaxInt
    updateTop k = dispatchState $ UpdateListZoom lz{ topKey = k }
    listMaxInt = fromIntegral listMax
    but f cls bdy prd = li_ [className cls', style endStyle] . e_ $ bdy
      where
        e_ | prd       = a_ (href "#" : ariaRole Button : clk)
           | otherwise = span_ [ariaDisabled True, ariaRole Button]
        clk | prd       = [onClick f]
            | otherwise = []
        cls' | prd       = cls
             | otherwise = cls <> " disabled"
    arr = span_ [ariaHidden True]

filterBox_ :: ListZoom -> Element
filterBox_ lz = form_ [ariaRole Search] . inputGroup_ [style $ marginX "auto"]
    $ input_ [ value $ maybe "" Job.nameText cur
             , onChange change
             , placeholder "Filter by name"
             , style $ width "20em" ]
  where
    cur = filterKey lz
    change _ val = dispatchState $ UpdateListZoom lz{ filterKey = case val of
        txt | T.null txt               -> Nothing
            | Just n <- Job.mkName txt -> Just n
            | otherwise                -> cur }

lzoom :: ListZoom -> Set Status -> ListView
lzoom ListZoom{..} = uncurry ListView . paginate . filterF . Set.toAscList
  where
    filterF | Just k <- filterKey = let t = Job.nameText k
                                    in filter $ (t `T.isInfixOf`) . Job.nameText . statusKey
            | otherwise           = id
    paginate | Just k <- topKey   = span $ (< k) . statusKey
             | otherwise          = ([],)

data NotAPrism s b = forall a. NotAPrism !(Lens' s a) !(a -> b) !(b -> Maybe a)
pset :: NotAPrism s b -> s -> b -> Maybe s
pset (NotAPrism l _ fba) s b = case fba b of
    Nothing -> Nothing
    Just a  -> Just $ s & l .~ a
{-# INLINE pset #-}
pget :: s -> NotAPrism s b -> b
pget s (NotAPrism l fab _) = fab $ s ^. l
{-# INLINE pget #-}
pid :: NotAPrism a a
pid = NotAPrism (lens id const) id Just
{-# INLINE pid #-}
jusp :: Lens' a s -> NotAPrism a s
jusp l = NotAPrism l id Just
{-# INLINE jusp #-}

addJob :: ReactView (Bool, SubmitStatus, PartialSpec)
addJob = defineView "addJob" go
  where
    go (disableSubmit, subSt, s@PartialSpec{..}) = form_ [className "form-horizontal", style $ marginTop "3ex"] $ do
        rowChangingInput "name" "Name"              input_     Nothing         $ NotAPrism pname id chkName
        rowChangingInput "cmd"  "Command"           input_     Nothing         $ jusp pcmd
        rowChangingInput "cpu"  "CPU"               wordInput_ (Just "dcores") $ minp 1  pcpu
        rowChangingInput "ram"  "RAM"               wordInput_ (Just "MB")     $ minp 10 pram
        rowChangingInput "disk" "Disk"              wordInput_ (Just "MB")     $ minp 10 pdisk
        rowChangingInput "time" "Time"              wordInput_ (Just "s")      $ minp 2  ptime
        rowChangingInput "ofp"  "Output file paths" textarea_  Nothing         $ linesNotPrism ppaths
        rowChangingInput "stdo" "Capture stdout"    checkbox_  Nothing         $ jusp pstdout
        rowChangingInput "deps" "Dependencies"      textarea_  Nothing         $ linesNotPrism pdeps
        formGroup_ [reactKey "submit"] . formUnlabelledRow_ $
            button_ [disabled cannotSubmit, onClick $ \_ _ -> submit] "Submit"
        case subSt of
            SubmitFailure err -> formGroup_ [reactKey "submitError"] . formUnlabelledRow_ . alert_ Danger . elemText $ showError err
            _                 -> pure ()
      where
        rowChangingInput :: (HasValue t) => JSString -> JSString -> Leaf' t -> Maybe JSString -> NotAPrism PartialSpec (Value t) -> Element
        rowChangingInput id_ lbl typ mAddStr p = formRow_ id_' lbl . addon $ input
          where
            id_' = "rowId" <> id_
            addon ip = case mAddStr of
                Nothing  -> ip
                Just str -> inputGroup_ $ ip <> inputAddon_ (elemStr str)
            input = typ [htmlId id_', value (s `pget` p), changing p]
        changing :: (HasValue t) => NotAPrism PartialSpec (Value t) -> Prop t
        changing p = onChange $ \_ val -> case pset p s val of
            Nothing -> []
            Just s' -> dispatchState $ UpdatePSpec s'
        chkName t | T.null t || isJust (Job.mkName t) = Just t
                  | otherwise                         = Nothing
        mkSpec = do
            name <- Job.mkName _pname
            cmd <- if T.null _pcmd then Nothing else Just _pcmd
            res <- Job.mkResources (fromIntegral _pram      %> mega Byte)
                                   (fromIntegral _pdisk     %> mega Byte)
                                   (fromIntegral _pcpu * 10 %> centi Core)
                                   (fromIntegral _ptime     %> Second)
            paths <- traverse (fmap Job.OutputPath . parseAbsFile . T.unpack) _ppaths
            deps <- traverse Job.mkName _pdeps
            pure $ Job.mkSpec name cmd res paths _pstdout deps
        submit = maybe [] (dispatchState . SubmitJob) mkSpec
        cannotSubmit = disableSubmit || subSt == Submitting || isNothing mkSpec
        linesNotPrism :: Lens' s [Text] -> NotAPrism s Text
        linesNotPrism l = NotAPrism l (T.intercalate "\n") (Just . parse)
          where
            parse "" = []
            parse t  = T.splitOn "\n" t
        minp :: (Ord a) => a -> Lens' s a -> NotAPrism s a
        minp m l = NotAPrism l id (\v -> if v >= m then Just v else Nothing)

addJob_ :: Bool -> SubmitStatus -> PartialSpec -> Element
addJob_ !d !st !s = viewWithKey addJob "addJob" (d, st, s)
-- NOTE: react-flux only does strict comparisons for 3-tuples and smaller

statusRow :: ReactView Status
statusRow = defineView "status-row" go
  where
    go s = tr_ $ do
        td "name"   . elemStr $ statusKeyStr s
        td "stage"  $ do
            elemStr . jsPack $ Job.majorStage stage
            case unStatus s ^. Job.satis of
                Job.Unsatisfiable r -> iconMeaning_ IconAlert "Warning" [style $ marginLeft "1em" <> textColour "orange", title msg]
                  where
                    msg = case r of
                        Job.DependenciesUnsatisfiable ns ->
                            jsUnpack $ "Dependencies " <> T.intercalate ", " (Job.nameText <$> ns) <> " are unsatisfiable"
                        Job.InsufficientResources -> "Insufficient resources to run job"
                _                       -> mempty
        td "output" . e_ . iconMeaning_ IconDownload $ "Download " <> statusKeyStr s <> " stdout"
        td "delete" . button_ [onClick $ \_ _ -> delete] . elemStr . jsPack $ show deleteType
      where
        stage      = unStatus s ^. Job.stage
        deleteType = bool Cancel Delete $ Job.isTerminationStage stage
        delete     = dispatchState $ DeleteJob (statusKey s) deleteType
        td k       = td_ [reactKey k]
        e_ | Job.isTerminationStage stage = a_ [href . jobStdoutUrl $ statusKey s, inNewTab, style sstyle] . ($ [])
           | otherwise                    = ($ [ariaRole Link, ariaDisabled True, style $ sstyle <> textColour "#ccc"])
          where
            sstyle = padX "1em" <> padY "1ex"

statusRow_ :: Status -> Element
statusRow_ !s = viewWithKey statusRow (statusKeyStr s) s

statusKeyStr :: Status -> JSString
statusKeyStr = jsUnpack . Job.nameText . statusKey

serverAuth :: URIAuth
serverAuth = URIAuth "" Awful.hostname $ ':' : show Awful.port

jobStdoutUrl :: Job.Name -> JSString
jobStdoutUrl name = jsPack $ uriToString id outputUri ""
  where
    outputUri = outputPath{ uriScheme = "http:", uriAuthority = Just serverAuth, uriPath = '/' : uriPath outputPath }
    outputPath = safeLink (Proxy :: Proxy API) (Proxy :: Proxy JobOutput) name $ Just Stdout

-- TODO: What happened to ConnectionError in ghcjs-servant-client?
convError :: ServantError -> SubmitError
convError (SC.ConnectionError (SomeException e))
    = ConnectionError . T.pack . displayException $ e
convError (SC.FailureResponse (HTTP.Status code _) _ body)
    | code == 0              = ConnectionError "Error connecting to server"
    | Just e <- decode' body = FailureResponse e
    | otherwise              = InvalidResponse "Could not decode server error response"
convError (SC.DecodeFailure err _ _)
    = InvalidResponse $ T.pack err
convError (SC.UnsupportedContentType _ _)
    = InvalidResponse "Server returned unsupported content type"
convError (SC.InvalidContentTypeHeader _ _)
    = InvalidResponse "Server returned invalid content type header"

showError :: SubmitError -> Text
showError (InvalidResponse msg) = msg
showError (FailureResponse e)   = displayError e
showError (ConnectionError msg) = msg

main :: IO ()
main = updateJobs *> reactRender "app-container" app ()
