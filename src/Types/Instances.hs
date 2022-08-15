{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Instances where

-- Prelude.
--import ClassyPrelude
--import Data.Aeson
--import Data.Aeson.Types (Value (String))
import Data.ByteString.Char8 qualified as B8
import Data.Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist.MongoDB
import Database.Persist.Sql qualified as S

--------------------------------------------------------------------------------

-- | Persistent instances for @UUID@.
instance PersistField UUID where
    toPersistValue uuid = PersistByteString . B8.pack . UUID.toString $ uuid
    fromPersistValue (PersistByteString uuidB8) =
        case UUID.fromString $ B8.unpack uuidB8 of
            Just uuid -> Right uuid
            Nothing -> Left (pack "Invalid UUID")
    fromPersistValue _ = Left (pack "Not PersistDBSpecific")

instance S.PersistFieldSql UUID where
    sqlType _ = SqlOther (pack "uuid")

--------------------------------------------------------------------------------

{- | Aeson @FromJSON@ and @ToJSON@ instances for @UUID@.
 instance FromJSON UUID where
  parseJSON = withText "UUID" $ \uuidStr ->
    case UUID.fromText uuidStr of
      Just uuid -> pure uuid
      Nothing   -> fail "Failed to parse UUID"
-}

--instance ToJSON UUID where
--  toJSON = String . UUID.toText
