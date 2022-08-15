{-# LANGUAGE OverloadedStrings #-}

module Logger (
    Config (..),
    adapt,
    defaultLogEnv,
    logMsg,
    runKatipT,
    KatipT (..),
    Katip (..),
    LogEnv,
    Severity (..),
) where

import Control.Monad.Logger (
    Loc,
    LogLevel (..),
    LogSource,
    ToLogStr,
 )
import Control.Monad.Logger qualified as Logger
import Data.Aeson qualified as A
import Katip (
    ColorStrategy (ColorIfTerminal),
    Katip (..),
    KatipT (..),
    LogEnv,
    LogStr,
    Namespace (Namespace),
    Severity (..),
    Verbosity (V2),
    defaultScribeSettings,
    initLogEnv,
    logMsg,
    logStr,
    mkHandleScribe,
    permitItem,
    registerScribe,
    runKatipT,
 )

--import qualified System.IO as IO
--import qualified Data.Text             as T
import System.Log.FastLogger qualified as FastLogger

--import           Data.Monoid              ((<>))
--import           Prelude               hiding (error, log)
--import Control.Applicative ((<|>), empty)

data Config = Config
    { cPath :: Maybe FilePath
    , cVerbosity :: Maybe Verbosity
    }
    deriving (Show)

instance Semigroup Config where
    Config p0 v0 <> Config p1 v1 = Config (p0 <|> p1) (v0 <|> v1)

instance Monoid Config where
    mempty = Config empty empty

--    Config p0 v0 `mappend` Config p1 v1 = Config (p0 <|> p1) (v0 <|> v1)

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Perservant.Logger.Config" $ \o ->
        Config
            <$> o A..:? "path"
            <*> o A..:? "verbosity"

data Handle = Handle
    { hConfig :: Config
    , hLoggerSet :: FastLogger.LoggerSet
    }

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
    env <- initLogEnv "mongo-servant" "production"
    registerScribe "stdout" handleScribe defaultScribeSettings env

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug = DebugS
fromLevel LevelInfo = InfoS
fromLevel LevelWarn = WarningS
fromLevel LevelError = ErrorS
fromLevel (LevelOther _) = NoticeS

{- | Transforms Katip logMsg into monadLoggerLog to be used inside
 MonadLogger monad
-}
adapt ::
    (ToLogStr msg, Applicative m, Katip m) =>
    (Namespace -> Severity -> Katip.LogStr -> m ()) ->
    Loc ->
    LogSource ->
    LogLevel ->
    msg ->
    m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Namespace [src]
    -- not sure how fast this is going to be
    logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
