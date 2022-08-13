{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}


module Server where

import Api (app)
import Api.Users (withHandle)
import qualified Database as D
--import Data.Maybe (fromMaybe)
import qualified Config as C
import qualified Data.Pool as Pool
--import Data.Text (Text, unpack)
import qualified Data.Text as Text
--import Data.Typeable (typeOf)
--import Models (doMigrations)
--import           Control.Monad.Trans      (liftIO)
import qualified Data.Yaml as Yaml
import qualified Katip
import qualified Logger as L
--import Network.Wai.Metrics (metrics, registerWaiMetrics)
import Safe (readMay)
import Say (say)
--import System.Environment (lookupEnv)
import Control.Concurrent ( killThread)
import Control.Monad.IO.Class (MonadIO (liftIO))
--import Metrics.Metrics
--import System.Metrics
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe
  ( MaybeT (..),
    runMaybeT,
  )
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Database.MongoDB.Connection (PortID (PortNumber))
import Database.Persist.MongoDB
  ( ConnectionPool,
    createMongoDBPool,
  )
import Logger
  (
    LogEnv,
    Severity (InfoS),
    logMsg,
    runKatipT,
  )
import Network.Wai ( Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Remote.Monitoring.Wai (forkServer, serverMetricStore, serverThreadId)
import Control.Exception.Safe
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setTimeout)
import Servant.Server 
import Servant.Auth.Server 

data ConfigApp = ConfigApp
  { cLogger :: L.Config,
    cServer :: C.ConfigApp,
    cDatabase :: D.Config
  }

instance Monoid ConfigApp where
  mempty =
    ConfigApp
      { cLogger = mempty,
        cServer = mempty,
        cDatabase   = mempty
      }

instance Semigroup ConfigApp where
  l <> r =
    ConfigApp
      { cLogger = cLogger l <> cLogger r,
        cServer = cServer l <> cServer r
        , cDatabase   = cDatabase   l <> cDatabase   r
      }

instance A.FromJSON ConfigApp where
  parseJSON = A.withObject "Mongo-Servant.Server" $ \o ->
    ConfigApp
      <$> o A..:? "logger" A..!= mempty
      <*> o A..:? "server" A..!= mempty
      <*> o A..:? "database" A..!= mempty

-- <*> o A..:? "database"    A..!= mempty

-- An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit

runAppDevel :: FilePath -> IO ()
runAppDevel f = do
  say "in runAppDevel"
  withConfig f $ \config -> do
    say "acquired config"
    cfg <-
      initialize config
        `finally` say "exited: initialize config"
    say "post-initialize"
    let warpSettings = defaultSettings
        portSettings = setPort (C.configPort config) warpSettings
        settings = setTimeout 55 portSettings
    runSettings settings cfg
      `finally` say "server is closed"

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: C.Config -> IO Application
initialize cfg = do
  say "initialize"
  --waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
  --say "wai metrics - oups !"
  let logger = setLogger (C.configEnv cfg)


  --say "run migrations"
  --bracket
  --  (say "starting to run migrations")
  --  (\_ -> say "migrations complete")
  --  $ \_ -> do
  --    say "actually running no migrations with mongoDB"
  --    say "okay all done"
      ctxCfg = C.cookieSettings cfg :. C.jwtSettings cfg :. EmptyContext 
      --  throwIO e
  --say "generate Swagger"
  --writeSwaggerJSON
  say "making app"
  pure . logger . app ctxCfg (C.cookieSettings cfg) (C.jwtSettings cfg) $ cfg -- metrics (C.configWaiMetrics cfg) . app $ cfg

withConfig :: FilePath -> (C.Config -> IO a) -> IO a
withConfig f action = do
  say "acquireConfig"
  --port <- lookupSetting "PORT" 8081

  --env <- lookupSetting "ENV" C.Development
  errOrConfig <- Yaml.decodeFileEither f
  ConfigApp {..} <- either (fail . show) return errOrConfig
  let port = fromMaybe 8000 $ C.cPort cServer
      env = C.convTextEnv $ fromMaybe "development" $ C.cEnv cServer
  say $ "on port:" <> show port
  say $ "on env: " <> show env
  --store <- newStore @AppMetrics
  bracket L.defaultLogEnv (\x -> say "closing katip scribes" >> Katip.closeScribes x) $ \logEnv -> do
    say "got log env"
    !pool <- makePool cDatabase env logEnv `onException` say "exception in makePool"
    myKey <- generateKey
    say "got pool "
    bracket (forkServer "localhost" 8083) (\x -> say "closing ekg" >> do killThread $ serverThreadId x) $ \ekgServer -> do
      say "forked ekg server"
      let store = serverMetricStore ekgServer
          
    --  waiMetrics <- registerWaiMetrics store `onException` say "exception in registerWaiMetrics"
    --  say "registered wai metrics"
      --metr <- initializeWith store
      --say "got metrics"
          cookieCfg = defaultCookieSettings{cookieIsSecure=NotSecure}
          jwtCfg = defaultJWTSettings myKey
      withHandle store (\hdl -> 
        action
          C.Config
            { configPool = pool,
              configEnv = env,
              configMetricsStore = store,
              --configWaiMetrics = waiMetrics,
              configLogEnv = logEnv,
              configPort = port,
              configEkgServer = serverThreadId ekgServer,
              hMetrics = hdl,
              cookieSettings = cookieCfg,
              jwtSettings = jwtCfg,
              jwtTimeout = 1001
            })

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: C.Config -> IO ()
shutdownApp cfg = do
  Katip.closeScribes (C.configLogEnv cfg)
  Pool.destroyAllResources (C.configPool cfg)
  -- Monad.Metrics does not provide a function to destroy metrics store
  -- so, it'll hopefully get torn down when async exception gets thrown
  -- at metrics server process
  --killThread (C.configEkgServer cfg)
  pass

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
--lookupSetting :: Read a => String -> a -> IO a
--lookupSetting env def = do
--  maybeValue <- lookupEnv env
--  case maybeValue of
--    Nothing ->
--      return def
--    Just str ->
--      maybe (handleFailedRead str) return (readMay str)
--  where
--    handleFailedRead str =
--      error $
--        toText . mconcat
--          [ "Failed to read [[",
--           str,
--            "]] for environment variable ",
--           env
--          ]

--tshow :: Show a => a -> Text
--tshow = toText @ByteString . show

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: C.Environment -> Middleware
setLogger C.Test = id
setLogger C.Development = logStdoutDev
setLogger C.Production = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
  -- todo: log proper request data
  logMsg "web" InfoS "todo: received some request"
  liftIO $ app req respond

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: D.Config -> C.Environment -> LogEnv -> IO ConnectionPool
makePool db env lenv =  
      let serv = fromMaybe "error" (D.cDBName db)
          host = toString . fromMaybe "error" $ D.cHostname db
      in runKatipT lenv (createMongoDBPool serv host (PortNumber 27017) Nothing (envPool env) 5 2000)
    -- If we don't have a correct database configuration, we can't
    -- handle that in the program, so we throw an IO exception. This is
    -- one example where using an exception is preferable to 'Maybe' or
    -- 'Either'.
--makePool _ C.Production env = do
  -- This function makes heavy use of the 'MaybeT' monad transformer, which
  -- might be confusing if you're not familiar with it. It allows us to
  -- combine the effects from 'IO' and the effect of 'Maybe' into a single
  -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
  -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
  -- give us a @Maybe a@, which would make the code quite a bit more
  -- verbose.
--  pool <- runMaybeT $ do
--    let keys = [ "host=", "port=","user=", "password=", "dbname="]
--        envs = [ "PGHOST","PGPORT","PGUSER","PGPASS","PGDATABASE"]
--    envVars <- traverse (MaybeT . lookupEnv) envs
--    let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
--    lift $ runKatipT env $ createMongoDBPool "servantMong" "localhost" (PortNumber 27017) Nothing (envPool C.Production) 5 2000
--  case pool of
    -- If we don't have a correct database configuration, we can't
    -- handle that in the program, so we throw an IO exception. This is
    -- one example where using an exception is preferable to 'Maybe' or
    -- 'Either'.
--    Nothing -> throwIO (userError "Database Configuration not present in environment.")
--    Just a -> return a

-- | The number of pools to use for a given environment.
envPool :: C.Environment -> Int
envPool C.Test = 1
envPool C.Development = 1
envPool C.Production = 8