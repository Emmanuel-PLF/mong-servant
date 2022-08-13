{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config
  ( Config (..),
    ConfigApp (..),
    Environment (..),
    AppT (..),
    convTextEnv
  )
where

--import Control.Applicative (empty, (<|>))
import Control.Concurrent (ThreadId)
import Control.Monad.Except (MonadError)
--import Control.Monad.IO.Class (MonadIO )
import Control.Monad.Logger (MonadLogger (..))
import System.Metrics
import Metrics.Metrics
--import Control.Monad.Reader (MonadReader, ReaderT, asks)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Database.Persist.MongoDB
  ( ConnectionPool
  )
import Logger
  ( Katip (..),
    KatipT,
    LogEnv,
    adapt,
    logMsg,
  )
import Network.Wai.Handler.Warp (Port)
--import Network.Wai.Metrics (WaiMetrics)
import Servant.Server (ServerError)
import Servant.Auth.Server (CookieSettings, JWTSettings)

data ConfigApp = ConfigApp
  { cPort :: Maybe Int,
    cDomain :: Maybe Text,
    cEnv    :: Maybe Text
    -- , cDatabase   :: Database.Config
  } deriving (Show)

instance Monoid ConfigApp where
  mempty =
    ConfigApp
      { cPort = empty,
        cDomain = empty,
        cEnv    = empty
        --, cDatabase   = mempty
      }

instance Semigroup ConfigApp where
  l <> r =
    ConfigApp
      { cPort = cPort l <|> cPort r,
        cDomain = cDomain l <|> cDomain r,
        cEnv = cEnv l <|> cEnv r
        --, cDatabase   = cDatabase   l <> cDatabase   r
      }

instance A.FromJSON ConfigApp where
  parseJSON = A.withObject "FromJSON Mongo-Servant.Config" $ \o ->
    ConfigApp
      <$> o A..:? "port"
      <*> o A..:? "domain"
      <*> o A..:? "environment"

-- <*> o A..:? "database"    A..!= mempty

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServerError m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
  { configPool :: ConnectionPool,
    configEnv :: Environment,
    configMetricsStore :: Store AllMetrics,
    --configWaiMetrics :: WaiMetrics,
    configEkgServer :: ThreadId,
    configLogEnv :: LogEnv,
    configPort :: Port,
    hMetrics :: MHandle,
    cookieSettings :: !CookieSettings,
    jwtSettings :: !JWTSettings,
    jwtTimeout :: !NominalDiffTime
  }

--instance Monad m => MonadMetrics (AppT m) where
--  getMetrics = asks Config.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
  getLogEnv = asks configLogEnv
  localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = adapt logMsg

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

convTextEnv :: T.Text -> Environment
convTextEnv "development" = Development
convTextEnv "test" = Test
convTextEnv "production" = Production

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
-- connStr :: BS.ByteString -> ConnectionString
-- connStr sfx = "host=localhost dbname=perservant" <> sfx <> " user=manu password=toto port=5432"
