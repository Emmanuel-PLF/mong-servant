{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Query.User where

-- Prelude.

-- Local imports.

import Control.Monad.IO.Class (MonadIO (liftIO))
--import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
--import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
--mport Database.Persist
--  ( Entity (Entity),
    --PersistStoreWrite (insert),
--  )
import Database.Persist.MongoDB 
 (
    Entity (..),
    PersistQueryRead (selectFirst),
    selectList,
    (==.)
 )
import Database.Persist.Class
import Database.Persist.TH ()
import Database (DB(..), Password (Password), User (User), Address (..))
import Types.BCrypt (hashPassword)

import Types.User

--------------------------------------------------------------------------------

-- | Insert a new user into the database.
insertUser :: Text -> UserResponse -> DB User
insertUser uHPass (UserResponse uEmail uName uPass uBio uIm uAdd) = do
  now <- liftIO getCurrentTime
  newUuid <- liftIO nextRandom

  hashedPw <- hashPassword uHPass
  (Entity userKey user) <- insertEntity $ User uName uEmail uBio Nothing (uAdd >>= convUserAddressDBAddress) newUuid
  _ <- insert $ Password hashedPw userKey
  pure user


-- | ConvertUserResponseAddress to Address DB
convUserAddressDBAddress :: UserAddress -> Maybe Address
convUserAddressDBAddress (UserAddress f s z) = 
  Just $ Address f s z  
--------------------------------------------------------------------------------
--getAllUsers :: DB [User]
--getAllUsers = do
--  au <- selectList [] []
--  pure au
--
---- | Retrieve a user and their hashed password from the database.
--getUserByEmail :: Text -> DB (Maybe (Entity User, Entity Password))
--getUserByEmail uEmail = fmap listToMaybe $
--  select $
--    from $ \(dbUser `InnerJoin` dbPass) -> do
--      on (dbUser ^. UserId ==. dbPass ^. PasswordUser)
--      where_ (dbUser ^. UserEmail ==. val uEmail)
--      pure (dbUser, dbPass)
--
---- | Retrieve a user and their hashed password from the database.
--getUserByUuid :: UUID -> DB (Maybe (Entity User))
--getUserByUuid uUuid = fmap listToMaybe $
--  select $
--    from $ \dbUser -> do
--      where_ (dbUser ^. UserUuid ==. val uUuid)
--      pure dbUser
