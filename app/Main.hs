{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.Trans.Either (runEitherT)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Eclogues.API (VAPI)
import qualified Eclogues.Job as Job
import Lens.Micro ((^.))
import React.Flux
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (..), Scheme (Http), client)

(getJobs :<|> _) = client (Proxy :: Proxy VAPI) $ BaseUrl Http "localhost" 8000

type ElementM = forall a. ReactElementM a ()

newtype State = State [Job.Status]

data Action = RefreshInit
            | RefreshReturn [Job.Status]

instance StoreData State where
    type StoreAction State = Action
    transform RefreshInit        s         = updateJobs *> pure s
    transform (RefreshReturn ss) (State _) = do
        async $ threadDelay 1000000 *> alterStore store RefreshInit
        pure $ State ss

updateJobs :: IO ()
updateJobs = void . async $ runEitherT getJobs >>= \case
    Right ss -> alterStore store $ RefreshReturn ss
    Left  e  -> print e

store :: ReactStore State
store = mkStore $ State []

app :: ReactView ()
app = defineControllerView "eclogues app" store $ \(State ss) () ->
    div_ $ appHeader_ <> jobList_ ss

appHeader_ :: ElementM
appHeader_ = h1_ "Eclogues Jobs"

jobList_ :: [Job.Status] -> ElementM
jobList_ ss = section_ ["id" $= "main"] $ do
    table_ $ do
        thead_ $ tr_ $ th_ "Name" <> th_ "Stage"
        tbody_ $ mapM_ job_ ss

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

main :: IO ()
main = updateJobs *> reactRender "app-container" app ()
