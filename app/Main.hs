{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Components

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Maybe (isNothing, isJust, listToMaybe)
import Data.Metrology.Computing (Byte (Byte), Core (Core), (%>))
import Data.Metrology.SI (Second (Second), mega, centi)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Eclogues.API (API)
import qualified Eclogues.Job as Job
import GHC.Generics (Generic)
import GHCJS.Marshal (FromJSVal)
import Lens.Micro (Lens', ASetter', (^.), (.~), (&))
import Lens.Micro.TH (makeLenses)
import qualified Network.HTTP.Types.Status as HTTP
import Path (parseAbsFile)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (..), Scheme (Http), client)
import qualified Servant.Client as SC
import Servant.Common.Req (ServantError)

import Debug.Trace (traceShow)

newtype Status = Status { unStatus :: Job.Status }
               deriving (Show, Generic, NFData)

type StatusKey = Job.Name

statusKey :: Status -> StatusKey
statusKey = (^. Job.name) . unStatus

instance Eq Status where
    (==) = (==) `on` statusKey

instance Ord Status where
    compare = comparing statusKey

data State = State { page         :: Page
                   , statuses     :: Set Status
                   , refreshError :: Maybe SubmitError
                   , pspec        :: PartialSpec
                   , submitStatus :: SubmitStatus
                   , listZoom     :: ListZoom }

data Page = JobList
          | AddJob
          deriving (Eq, Generic, NFData)

data ListZoom = ListZoom { filterKey :: Maybe StatusKey
                         , topKey    :: Maybe StatusKey }
              deriving (Generic, NFData)

data ListView = ListView { prevSpan :: [Status]
                         , curSpan  :: [Status] }

data SubmitStatus = NotSubmitted
                  | Submitting
                  | SubmitFailure SubmitError
                  deriving (Eq, Generic, NFData)

data SubmitError = InvalidResponse Text
                 | ConnectionError Text
                 | FailureResponse Text
                 deriving (Show, Eq, Generic, NFData)

data PartialSpec = PartialSpec { _pname   :: Text
                               , _pcmd    :: Text
                               , _pram    :: Word
                               , _pdisk   :: Word
                               , _pcpu    :: Word
                               , _ptime   :: Word
                               , _ppaths  :: [Text]
                               , _pstdout :: Bool
                               , _pdeps   :: [Text] }
                 deriving (Show, Generic, NFData)

$(makeLenses ''PartialSpec)

getJobs :: EitherT ServantError IO [Job.Status]
createJob' :: Job.Spec -> EitherT ServantError IO ()
(     getJobs
 :<|> _
 :<|> _
 :<|> _
 :<|> _
 :<|> _
 :<|> _
 :<|> createJob'
 :<|> _) = client (Proxy :: Proxy API) $ BaseUrl Http Awful.hostname Awful.port

createJob :: Job.Spec -> IO (Maybe SubmitError)
createJob j = either (Just . convError) (const Nothing) <$> runEitherT (createJob' j)

defPartialSpec :: PartialSpec
defPartialSpec = PartialSpec "" "" 10 10 1 60 [] False []

defListZoom :: ListZoom
defListZoom = ListZoom Nothing Nothing

listMax :: Word
listMax = 2

data Action = RefreshInit
            | RefreshReturn (Either SubmitError (Set Status))
            | SwitchPage Page
            | UpdateListZoom ListZoom
            | UpdatePSpec PartialSpec
            | SubmitJob Job.Spec
            | SubmitReturn (Maybe SubmitError)
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
        _ <- async $ alterStore store . SubmitReturn =<< createJob j
        pure $ s{ submitStatus = Submitting }
    transform (SubmitReturn Nothing) s  = pure $ s{ submitStatus = NotSubmitted
                                                  , pspec = defPartialSpec
                                                  , page = JobList }
    transform (SubmitReturn (Just e)) s = pure $ s{ submitStatus = SubmitFailure e }

-- TODO: handle errors more gracefully
updateJobs :: IO ()
updateJobs = void . async $ runEitherT getJobs >>= \r ->
    alterStore store . RefreshReturn $ bimap convError (Set.fromList . fmap Status) r

store :: ReactStore State
store = mkStore $ State JobList Set.empty Nothing defPartialSpec NotSubmitted defListZoom

dispatchState :: Action -> [SomeStoreAction]
dispatchState a = [SomeStoreAction store a]

app :: ReactView ()
app = defineControllerView "eclogues app" store $ \s () ->
    pageContainer_ $ do
        appHeader_
        mapM_ (alert_ Danger . elemText . T.unpack . showError) $ refreshError s
        links_ $ page s
        section_ [htmlId "main"] $ pageElement_ s

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
    tab :: String -> Page -> Element
    tab lbl dest = tab_ (dest == cur) . a_ [href "#", goto dest] $ elemText lbl
    goto :: Page -> Prop Link
    goto dest = onClick $ \_ _ -> dispatchState $ SwitchPage dest

statusList_ :: Set Status -> ListZoom -> Element
statusList_ ss lz = do
    pagination_ lz lv
    table_ $ do
        thead_ . tr_ $ th_ [style $ width "50%"] "Name" <> th_ "Stage"
        tbody_ . mapM_ statusRow_ $ take (fromIntegral listMax) (curSpan lv)
  where
    lv = lzoom lz ss

pagination_ :: ListZoom -> ListView -> Element
pagination_ lz@ListZoom{..} ListView{..} = ul_ [className "pager", style topStyle] $ do
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
    but f cls bdy prd = li_ [className cls', style endStyle]
        . a_ (href "#" : clk) $ bdy
      where
        clk | prd       = [onClick f]
            | otherwise = []
        cls' | prd       = cls
             | otherwise = cls <> " disabled"
    arr = span_ [ariaHidden True]

filterBox_ :: ListZoom -> Element
filterBox_ lz = form_ . inputGroup_ [style $ marginX "auto"]
    $ input_ [ inputType "text"
             , value $ maybe "" Job.nameText cur
             , onChange change
             , placeholder "Filter by name"
             , style $ width "20em" ]
  where
    cur = filterKey lz
    change evt = dispatchState $ UpdateListZoom lz{ filterKey = case newValue evt of
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

addJob_ :: Bool -> SubmitStatus -> PartialSpec -> Element
addJob_ disableSubmit subSt s@PartialSpec{..} = form_ [className "form-horizontal", style $ marginTop "3ex"] $ do
    rowChangingInput "name" "Name"    "text"   Nothing pname chkName
    rowChangingInput "cmd"  "Command" "text"   Nothing pcmd  Just
    rowChangingInput "cpu"  "CPU"     "number" (Just "dcores") pcpu  Just
    rowChangingInput "ram"  "RAM"     "number" (Just "MB")     pram  Just
    rowChangingInput "disk" "Disk"    "number" (Just "MB")     pdisk Just
    rowChangingInput "time" "Time"    "number" (Just "s")      ptime Just
    formRow_ "rowIdofp" "Output file paths" $
        textarea_ [htmlId "rowIdofp", value $ interlines _ppaths, changing ppaths (Just . splitLines)]
    rowChangingInput "stdo" "Capture stdout" "checkbox" Nothing pstdout Just
    formRow_ "rowIddeps" "Dependencies" $
        textarea_ [htmlId "rowIddeps", value $ interlines _pdeps, changing pdeps (Just . splitLines)]
    formGroup_ . formUnlabelledRow_ $
        button_ [disabled cannotSubmit, onClick $ \_ _ -> submit] "Submit"
    case subSt of
        SubmitFailure err -> p_ . elemText . T.unpack $ showError err
        _                 -> pure ()
  where
    rowChangingInput :: (FromJSVal t, ToJSON a) => Text -> String -> Text -> Maybe String -> Lens' PartialSpec a -> (t -> Maybe a) -> Element
    rowChangingInput id_ lbl typ mAddStr l = formRow_ id_' lbl . addon . changingInput id_' typ l
      where
        id_' = "rowId" <> id_
        addon ip = case mAddStr of
            Nothing  -> ip
            Just str -> inputGroup_ $ ip <> inputAddon_ (elemText str)
    changingInput :: (FromJSVal t, ToJSON a) => Text -> Text -> Lens' PartialSpec a -> (t -> Maybe a) -> Element
    changingInput id_ typ l validate = input_ [htmlId id_, inputType typ, jsonValue (s ^. l), changing l validate]
    changing :: (FromJSVal t, HasValue u) => ASetter' PartialSpec a -> (t -> Maybe a) -> Prop u
    changing l validate = onChange $ \evt -> case validate $ newValue evt of
        Nothing -> []
        Just v  -> dispatchState . UpdatePSpec $ s & l .~ v
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
    submit = traceShow s $ maybe [] (dispatchState . SubmitJob) mkSpec
    cannotSubmit = disableSubmit || subSt == Submitting || isNothing mkSpec
    interlines = T.intercalate "\n"
    splitLines = T.splitOn "\n"

statusRow :: ReactView Status
statusRow = defineView "status-row" $ \s ->
    let name = statusKeyStr s
    in  tr_ $ td "name" name <> td "stage" (Job.majorStage $ (unStatus s) ^. Job.stage)
  where
    td :: String -> String -> Element
    td k = td_ [reactKey k] . elemText

statusRow_ :: Status -> Element
statusRow_ s = viewWithKey statusRow (statusKeyStr s) s mempty

statusKeyStr :: Status -> String
statusKeyStr = T.unpack . Job.nameText . statusKey

-- TODO: What happened to ConnectionError in ghcjs-servant-client?
convError :: ServantError -> SubmitError
convError (SC.FailureResponse (HTTP.Status code bmsg) _ _)  -- TODO: decode Eclogues error
    | code == 0                     = ConnectionError "Error connecting to server"
    | Right msg <- decodeUtf8' bmsg = FailureResponse msg
    | otherwise                     = InvalidResponse "Could not decode server response"
convError (SC.DecodeFailure err _ _)
    = InvalidResponse $ T.pack err
convError (SC.UnsupportedContentType _ _)
    = InvalidResponse "Server returned unsupported content type"
convError (SC.InvalidContentTypeHeader _ _)
    = InvalidResponse "Server returned invalid content type header"

showError :: SubmitError -> Text
showError (InvalidResponse msg) = msg
showError (FailureResponse msg) = msg
showError (ConnectionError msg) = msg

main :: IO ()
main = updateJobs *> reactRender "app-container" app ()
