{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Types.User where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    defaultOptions,
  )
import Data.Aeson.Types (Parser)

--import Data.OpenApi
-- ( HasDescription (description),
--    HasEmail (..),
--    HasName (..),
--    HasPassword (..),
--    HasSchema (schema),
--    ToSchema (..),
--    defaultSchemaOptions,
--    genericDeclareNamedSchema,
--  )
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.MongoDB (PersistField)
import GHC.Generics (Generic)
import Lens.Micro (mapped, (&), (?~))
import Lens.Micro.TH (makeFields)
import Types.Token (JWTText)
import Utils.Aeson
  ( gCustomParseJSON,
    gCustomToJSON,
    gSnakeCase,
    unwrapJson,
    wrapJson,
  )

-- NOTE: All these newtype wrappers might be superfluous; worth thinking on.
--------------------------------------------------------------------------------

-- | Newtype wrapper around @Text@ for a user' email.
newtype UEmail = UEmail Text
  deriving (Eq, PersistField, FromJSON, ToJSON, Show)

-- | Newtype wrapper around @Text@ for a username.
newtype UName = UName Text
  deriving (Eq, PersistField, FromJSON, ToJSON, Show, Generic)

-- NOTE: Is this even really necessary? It's nice at least that it doesn't

-- | Newtype wrapper around @Text@ for a user's plaintext password.
newtype UPlainText = UPlainText {fromUPlainText :: Text}
  deriving (Eq, FromJSON)

-- | Newtype wrapper around @Text@ for a user's bio.
newtype UBio = UBio Text
  deriving (Eq, PersistField, FromJSON, ToJSON, Show)

-- | Newtype wrapper around @Text@ for a user's image URL.
newtype UImage = UImage Text
  deriving (Eq, PersistField, FromJSON, ToJSON, Show)

--------------------------------------------------------------------------------

-- | User login JSON request.
data UserLogin = UserLogin
  { userLoginLlogin :: !Text,
    userLoginPassword :: !Text
  }
  deriving (Generic)

instance FromJSON UserLogin where
  parseJSON = gCustomParseJSON $ gSnakeCase defaultOptions

--------------------------------------------------------------------------------

-- | User registration JSON request.
data UserRegister = UserRegister
  { userRegisterEmail :: !Text,
    userRegisterNameu :: !Text,
    userRegisterPasswordu :: !Text,
    userRegisterBio :: !Text
  }
  deriving (Generic, Typeable)

instance FromJSON UserRegister where
  parseJSON = gCustomParseJSON $ gSnakeCase defaultOptions

--instance ToSchema UserRegister where
--  declareNamedSchema proxy =
--    genericDeclareNamedSchema defaultSchemaOptions proxy
 --     & mapped . schema . description ?~ "Ajouter un nouvel utilisateur"

--------------------------------------------------------------------------------

-- | User update JSON request.
data UserUpdate = UserUpdate
  { userUpdateEmail :: !(Maybe UEmail),
    userUpdateName :: !(Maybe UName),
    userPassword :: !(Maybe UPlainText),
    userBio :: !(Maybe UBio),
    userImage :: !(Maybe UImage)
  }
  deriving (Generic)

instance FromJSON UserUpdate where
  parseJSON = unwrapUser $ gCustomParseJSON $ gSnakeCase defaultOptions

--------------------------------------------------------------------------------
data UserAddress = UserAddress
  { first :: !Text,
    second :: !Text,
    zipcode :: !Int
  }
  deriving (Generic, Typeable, Eq, Show)

instance ToJSON UserAddress
instance FromJSON UserAddress

--instance ToSchema UserAddress where
--  declareNamedSchema proxy =
--    genericDeclareNamedSchema defaultSchemaOptions proxy
--      & mapped . schema . description ?~ "Address du user"

data UserResponseBis = UserResponseBis
  { addName :: !Text,
    addAddress :: !UserAddress
  }
  deriving (Generic, Typeable, Eq, Show)

--instance ToSchema UserResponseBis where
--  declareNamedSchema proxy =
--    genericDeclareNamedSchema defaultSchemaOptions proxy
--      & mapped . schema . description ?~ "Test structure inclue"

instance ToJSON UserResponseBis

-- | User JSON response.
data UserResponse = UserResponse
  { userResponseEmail :: !Text,
    userResponseName :: !Text,
    userResponsePassword :: !(Maybe Text),
    userResponseBio :: !(Maybe Text),
    userResponseImage :: !(Maybe Text),
    userResponseAddress :: !(Maybe UserAddress)
  }
  deriving (Generic, Typeable)

instance ToJSON UserResponse where
  toJSON = gCustomToJSON $ gSnakeCase defaultOptions

instance FromJSON UserResponse where
  parseJSON = gCustomParseJSON $ gSnakeCase defaultOptions

--instance ToSchema UserResponse where
--  declareNamedSchema proxy =
--    genericDeclareNamedSchema defaultSchemaOptions proxy
--      & mapped . schema . description ?~ "Infos sur un utilisateur"

--------------------------------------------------------------------------------

-- | User JSON response.
newtype UserToken = UserToken
  { userTokenAccess :: Text
  }
  deriving (Generic, Typeable)

instance ToJSON UserToken where
  toJSON = gCustomToJSON $ gSnakeCase defaultOptions

--------------------------------------------------------------------------------

-- | Unwrap the top-level "user" object from some JSON before deserializing it.
unwrapUser ::
  forall a b.
  (FromJSON a, Typeable b) =>
  (a -> Parser b) ->
  Value ->
  Parser b
unwrapUser = unwrapJson "user"

-- | Wrap the data type to-be-serialized in a top-level "user" object.
wrapUser :: ToJSON a => a -> Value
wrapUser = wrapJson "user"

--------------------------------------------------------------------------------

-- | Generate field accessors.
makeFields ''UserLogin
makeFields ''UserResponse
--makeFields ''UserUpdate
--makeFields ''UserToken
