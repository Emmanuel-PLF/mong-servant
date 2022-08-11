{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Control.Applicative (empty, (<|>))
import qualified Config as C
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
--import Data.Time (UTCTime)
import Data.UUID (UUID)
--import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)

import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH
--import Say (say)
import Types.BCrypt (BCrypt)
import Types.Instances ()

import Types.User ()

let mongoSettings = mkPersistSettings (ConT ''MongoContext) --{mpsGeneric = False}
 in share
      [mkPersist mongoSettings]
      [persistLowerCase|
User json
    name  Text
    email Text
    bio   Text  Maybe
    image Text  Maybe
    address Address Maybe
    uuid      UUID
    deriving Eq Show

Address json
   first Text
   second Text
   zipcode Int
   deriving Eq Show

Password json
    hash      BCrypt
    user      UserId
    deriving Eq Show

Command json
    num         Int
    user        UserId
    description Text
    UniqueCommandNum num
    deriving Eq Show

|]

--type DB a = SqlPersistT IO a
type DB a = Action IO a

--doMigrations :: SqlPersistT IO ()
--doMigrations = do
--  liftIO $ say "in doMigrations, running?"
--  runMigration migrateAll
--  liftIO $ say "already run"


data Config = Config
  { cDBName :: Maybe T.Text,
    cHostname :: Maybe T.Text
  } deriving (Show)

instance Monoid Config where
  mempty =
    Config
      { cDBName = empty,
        cHostname = empty
      }

instance Semigroup Config where
  l <> r =
    Config
      { cDBName = cDBName l <|> cDBName r,
        cHostname = cHostname l <|> cHostname r
      }

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Mongo-Servant.Database.Config" $ \o ->
    Config
      <$> o A..:? "dbname"
      <*> o A..:? "hostname"

runDb :: (MonadReader C.Config m, MonadIO m) => DB b -> m b
runDb query = do
  pool <- asks C.configPool
  liftIO $ runMongoDBPool master query pool
