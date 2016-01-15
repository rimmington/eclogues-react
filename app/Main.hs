{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe, isNothing)
import Data.Metrology.Computing ((%>), Byte (Byte), Core (Core))
import Data.Metrology.SI (Second (Second), mega)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Eclogues.API (API)
import qualified Eclogues.Job as Job
import GHC.Generics (Generic)
import GHCJS.Marshal (FromJSRef)
import Lens.Micro (Lens', ASetter', (^.), (.~), (&))
import Lens.Micro.TH (makeLenses)
import qualified Network.HTTP.Types.Status as HTTP
import Path (parseAbsFile)
import React.Flux
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (..), Scheme (Http), client)
import qualified Servant.Client as SC
import Servant.Common.Req (ServantError)

import Debug.Trace (traceShow)

type ElementM = ReactElementM ViewEventHandler ()
type Handler  = PropertyOrHandler ViewEventHandler

data State = State { page         :: Page
                   , jobs         :: [Job.Status]
                   , pspec        :: PartialSpec
                   , submitStatus :: SubmitStatus }

data Page = JobList
          | AddJob
          deriving (Generic, NFData)

data SubmitStatus = NotSubmitted
                  | Submitting
                  | SubmitFailure SubmitError
                  deriving (Eq, Generic, NFData)

data SubmitError = InvalidResponse Text
                 | ConnectionError Text
                 | FailureResponse Text
                 deriving (Eq, Generic, NFData)

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
 :<|> _) = client (Proxy :: Proxy API) $ BaseUrl Http "localhost" 8000

createJob :: Job.Spec -> IO (Maybe SubmitError)
createJob j = either (Just . convError) (const Nothing) <$> runEitherT (createJob' j)

defPartialSpec :: PartialSpec
defPartialSpec = PartialSpec "" "" 10 10 1 60 [] False []

defResources :: Job.Resources
defResources = fromMaybe (error "invalid default resources") $
    Job.mkResources (10 %> mega Byte) (10 %> mega Byte) (1 %> Core) (60 %> Second)

data Action = RefreshInit
            | RefreshReturn [Job.Status]
            | SwitchPage Page
            | UpdatePSpec PartialSpec
            | SubmitJob Job.Spec
            | SubmitReturn (Maybe SubmitError)
            deriving (Generic, NFData)

instance StoreData State where
    type StoreAction State = Action
    transform RefreshInit        s    = updateJobs *> pure s
    transform (RefreshReturn njobs) s = do
        _ <- async $ threadDelay 1000000 *> alterStore store RefreshInit
        pure $ s{ jobs = njobs }
    transform (SwitchPage np) s         = pure $ s{ page = np }
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
updateJobs = void . async $ runEitherT getJobs >>= \case
    Right ss -> alterStore store $ RefreshReturn ss
    Left  e  -> print e

store :: ReactStore State
store = mkStore $ State JobList [] defPartialSpec NotSubmitted

dispatchState :: Action -> [SomeStoreAction]
dispatchState a = [SomeStoreAction store a]

app :: ReactView ()
app = defineControllerView "eclogues app" store $ \s () ->
    div_ $ appHeader_ <> links_ <> section_ ["id" $= "main"] (pageElement_ s)

pageElement_ :: State -> ElementM
pageElement_ State{..} = case page of
    JobList -> jobList_ jobs
    AddJob  -> addJob_ submitStatus pspec

appHeader_ :: ElementM
appHeader_ = h1_ "Eclogues Jobs"

links_ :: ElementM
links_ = p_ $ do
    link "Job List" JobList
    link "Add Job"  AddJob
  where
    link :: String -> Page -> ElementM
    link lbl dest = a_ ["href" $= "#", goto dest] $ elemText lbl
    goto :: Page -> Handler
    goto dest = onClick $ \_ _ -> dispatchState $ SwitchPage dest

jobList_ :: [Job.Status] -> ElementM
jobList_ ss = table_ $ do
    thead_ $ tr_ $ th_ "Name" <> th_ "Stage"
    tbody_ $ mapM_ job_ ss

addJob_ :: SubmitStatus -> PartialSpec -> ElementM
addJob_ subSt s@PartialSpec{..} = form_ $ do
    row "Name" $ changingInput "text" pname chkName
    row "Command" $ changingInput "text" pcmd Just
    row "CPU" $ changingInput "number" pcpu Just
    row "RAM" $ changingInput "number" pram Just
    row "Disk" $ changingInput "number" pdisk Just
    row "Time" $ changingInput "number" ptime Just
    row "Output file paths" $ textarea_ ["value" $= interlines _ppaths, changing ppaths (Just . splitLines)] empty
    row "Capture stdout" $ changingInput "checkbox" pstdout Just
    row "Dependencies" $ textarea_ ["value" $= interlines _pdeps, changing pdeps (Just . splitLines)] empty
    button_ ["disabled" @= cannotSubmit, onClick $ \_ _ -> submit] "Submit"
    case subSt of
        SubmitFailure err -> p_ . elemText . T.unpack $ showError err
        _                 -> pure ()
  where
    row :: String -> ElementM -> ElementM
    row lbl e = label_ $ elemText lbl <> e
    changingInput :: (FromJSRef t, ToJSON a) => Text -> Lens' PartialSpec a -> (t -> Maybe a) -> ElementM
    changingInput typ l validate = input_ ["type" $= typ, "value" @= (s ^. l), changing l validate]
    changing :: (FromJSRef t) => ASetter' PartialSpec a -> (t -> Maybe a) -> Handler
    changing l validate = onChange $ \evt -> case validate $ target evt "value" of
        Nothing -> []
        Just v  -> dispatchState . UpdatePSpec $ s & l .~ v
    chkName = fmap Job.nameText . Job.mkName
    empty = pure ()
    mkSpec = do
        name <- Job.mkName _pname
        res <- Job.mkResources (fromIntegral _pram  %> mega Byte)
                               (fromIntegral _pdisk %> mega Byte)
                               (fromIntegral _pcpu  %> Core)
                               (fromIntegral _ptime %> Second)
        paths <- traverse (fmap Job.OutputPath . parseAbsFile . T.unpack) _ppaths
        deps <- traverse Job.mkName _pdeps
        pure $ Job.mkSpec name _pcmd res paths _pstdout deps
    submit = traceShow s $ maybe [] (dispatchState . SubmitJob) mkSpec
    cannotSubmit = subSt == Submitting || isNothing mkSpec
    interlines = T.intercalate "\n"
    splitLines = T.splitOn "\n"

job :: ReactView Job.Status
job = defineView "job" $ \s ->
    let name = jobNameString s
    in  tr_ $ td "name" name <> td "stage" (Job.majorStage $ s ^. Job.stage)
  where
    td :: String -> String -> ElementM
    td k = td_ ["key" @= k] . elemText

job_ :: Job.Status -> ElementM
job_ s = viewWithKey job (jobNameString s) s mempty

jobNameString :: Job.Status -> String
jobNameString = T.unpack . Job.nameText . (^. Job.name)

-- TODO: What happened to ConnectionError in ghcjs-servant-client?
convError :: ServantError -> SubmitError
convError (SC.FailureResponse (HTTP.Status _ msg) _ _)  -- TODO: decode Eclogues error
    = either (const $ InvalidResponse "Could not decode server response") FailureResponse $ decodeUtf8' msg
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
