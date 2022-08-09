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
import Database.Persist
  ( Entity (Entity),
    PersistStoreWrite (insert),
  )
import Database.Persist.MongoDB ()
import Database.Persist.TH ()
import Database (DB(..), Password (Password), User (User))
import Types.BCrypt (hashPassword)

import Types.User

--------------------------------------------------------------------------------

-- | Insert a new user into the database.
insertUser :: Text -> UserResponse -> DB (Entity User)
insertUser uHPass (UserResponse uEmail uName uPass uBio uIm uAdd) = do
  now <- liftIO getCurrentTime
  newUuid <- liftIO nextRandom

  hashedPw <- hashPassword uHPass
  userKey <- insert $ User uName uEmail uBio Nothing newUuid

  _ <- insert $ Password hashedPw userKey

  --userRec <- get userKey
  pure (Entity userKey $ User uName uEmail uBio Nothing newUuid)
  --pure userKey

--------------------------------------------------------------------------------
--getAllUsers :: DB [Entity User]
--getAllUsers =
--  select $
--    from $ \dbUser -> do
--      pure dbUser
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
