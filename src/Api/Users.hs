{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TemplateHaskell #-}

module Api.Users where

-- Prelude.
--import ClassyPrelude hiding (hash)
import Config (AppT (..), Config(..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Reader (asks, liftIO)
--import Control.Monad.Catch (throwM)
import Database.Persist.MongoDB 
 (
    Entity (Entity),
    PersistQueryRead (selectFirst),
    selectList,
    (==.)
 )
import qualified Database as Md
-- Servant imports.

-- Local imports.
--import Foundation
--import Logging
--import Model
import Query.User
import Servant
import Lens.Micro ((^.))
import Servant.Auth.Server
import Types.BCrypt
import Types.Token
--import Types.User hiding (userBio, userImage)
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BSL
--import Data.Time (NominalDiffTime, addUTCTime)
import Data.Time.Clock
import qualified Database as D
import Types.User

import qualified System.Metrics as SM
import System.Metrics.Counter
import Metrics.Metrics as M
--import Data.OpenApi
--  ( HasDescription (description),
--    HasEmail (email),
--    HasInfo (info),
--    HasLicense (license),
--    HasName (name),
--    HasPassword (password),
--    HasServers (servers),
--    HasTitle (title),
--    HasUrl (url),
--    HasVersion (version),
--    OpenApi,
--    URL (URL),
--  )
--------------------------------------------------------------------------------

-- | Servant type-level representation of the "users" route fragment.
type UsersApi auths = (Auth auths Token :> ProtectedApi) :<|> UnprotectedApi

-- | Handler function for the "users" route fragment.
usersHandler :: MonadIO m => ServerT (UsersApi auths) (AppT m)
usersHandler = protected :<|> unprotected

--------------------------------------------------------------------------------

-- | Type-level representation of the endpoints protected by 'Auth'.
type ProtectedApi =
  "users" :> Get '[JSON] [UserResponse]
    :<|> "users" 
      :> "register"
      :> ReqBody '[JSON] UserResponse
      :> Post '[JSON] UserResponse
    :<|> "users"
      :> Capture "name" Text
      :> Get '[JSON] UserResponse

adminLoginApi :: Proxy (UsersApi '[JWT])
adminLoginApi = Proxy

withHandle :: SM.Store SM.AllMetrics -> (M.Handle -> IO a) -> IO a
withHandle s f = do
  auc <- SM.createCounter (SM.Metric @"perservant.allusers") () s
  suc <- SM.createCounter (SM.Metric @"perservant.singleuser") () s
  cuc <- SM.createCounter (SM.Metric @"perservant.createuser") () s
  luc <- SM.createCounter (SM.Metric @"perservant.login") () s
  f $ M.Handle suc auc cuc luc


-- | Check authentication status and dispatch the request to the appropriate
-- endpoint handler.
protected :: MonadIO m => AuthResult Token -> ServerT ProtectedApi (AppT m)
protected (Authenticated t) = allUsers t :<|> register t :<|> singleUser
protected _ = throwAll err401


-- | Registration endpoint handler.
register :: MonadIO m => Token -> UserResponse -> AppT m UserResponse
register _ userReg = do
  logDebugNS "register" "Enter"
  increment M.hCreateUserC
  case userReg ^. password of
    Nothing -> throwError err404
    Just p -> do
            dbUser <- D.runDb $ insertUser p userReg 
            mkUserResponse dbUser

-- | Returns all users in the database.
allUsers :: MonadIO m => Token -> AppT m [UserResponse]
allUsers _ = do
  increment M.hAllUsersC
  logDebugNS "web" "allUsers"
  au <- D.runDb (selectList [] [])
  mapM mkUserResponse au

-- | Returns a user by name or throws a 404 error.
singleUser :: MonadIO m => Text -> AppT m UserResponse
singleUser str = do
  increment M.hSingleUserC
  logDebugNS "web" "singleUser"
  maybeUser <- D.runDb (selectFirst [Md.UserName ==. str] [])
  case maybeUser of
    Nothing ->
      throwError err404
    Just dbUser ->
      mkUserResponse dbUser

--------------------------------------------------------------------------------

-- | Type-level representation of the endpoints not protected by 'Auth'.
type UnprotectedApi =
  "users"
    :> "login"
    :> ReqBody '[JSON] UserLogin
    :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserToken)

-- | Dispatch the request to the appropriate endpoint handler.
unprotected :: MonadIO m => ServerT UnprotectedApi (AppT m)
unprotected = login

login :: MonadIO m => UserLogin -> AppT m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserToken)
login userLogin = do
  -- Get the user and password associated with this email, if they exist.
  logDebugNS "login" "Avant Appel DB"
  increment M.hLoginC
  maybeUserPass <- D.runDb $ selectFirst [Md.UserEmail ==. userLogin ^. llogin] []
  case maybeUserPass of
    Nothing -> do logDebugNS "login" "pas de mail"
                  throwError err404
    Just
      ( Entity personId person) -> do
        ppass <- D.runDb $ selectFirst [Md.PasswordUser ==. personId] []
        case ppass of
          Nothing -> do logDebugNS "login" "pas de password"
                        throwError err404
          Just ( Entity _ pass) -> do 
            tok  <- mkToken (userLogin ^. password) ( D.passwordHash pass) person
            utok <- mkTokenResponse tok
            settings <- asks jwtSettings
            cooksett <- asks cookieSettings
            mAppCook <- liftIO $ acceptLogin cooksett settings tok
            case mAppCook of
              Nothing -> do throwError err401
              Just ac -> do
                pure $ ac utok


--mkUserResponse userLogin (passwordHash dbPass) dbUser logAction

--------------------------------------------------------------------------------


-- | increment a counter
increment :: MonadIO m => (Handle -> Counter) -> AppT m ()
increment f = do
  h <- asks hMetrics
  liftIO $ inc $ f h 


-- | Return a token for a given user if the login password is valid when
-- compared to the hash in the database; throw 401 if the user's password
-- is invalid
mkToken :: MonadIO m => Text -> BCrypt -> D.User -> AppT m Token
mkToken pass hashed dbUser = do
  -- Validate the stored hash against the plaintext password
  isValid <- validatePassword pass hashed

  -- If the password isn't valid, throw a 401
  -- TODO - maybe validatePassword should return an Either so that when
  -- validation fails internally, we can throw a 500.
  if isValid then pure () else throwError err401
  logDebugNS "mkToken" "Validate Password" 
  pure $ Token (D.userUuid dbUser)

-- | Return a textual view of a JWT from a token, valid for a given duration
-- of seconds
mkJWT :: MonadIO m => Token -> NominalDiffTime -> AppT m JWTText
mkJWT token duration = do
--  -- Try to make a JWT with the settings from the Reader environment.
  settings <- asks jwtSettings
  expires <- liftIO $ Just . addUTCTime duration <$> getCurrentTime
  tryJWT <- liftIO $ makeJWT token settings expires

  case tryJWT of
--    -- If JWT generation failed, log the error and throw a 500
    Left e -> throwError err500
    Right lazyJWT -> pure . JWTText . decodeUtf8 . BSL.toStrict $ lazyJWT
--
-- | Generate a 'UserResponse' with an expiring token (defined in 'Config'),
-- logging to 'Katip' with the given @logAction@ function.
mkTokenResponse :: MonadIO m => Token -> AppT m UserToken
mkTokenResponse tk = do
  timeot <- asks jwtTimeout
  --jwt <- mkJWT tk timeot
  pure $ UserToken "SUCCESS"

mkUserResponse ::
  MonadIO m =>
  Entity D.User ->
  AppT m UserResponse
mkUserResponse
  (Entity _ dbUser) = do
    --timeot <- asks jwtTimeout
    --tok <- mkToken (dbUser ^. password) hashedPw dbUser
    --jwt <- mkJWT tok timeot
    pure $
      UserResponse
        (D.userEmail dbUser)
        (D.userName dbUser)
        Nothing
        (D.userBio dbUser)
        (D.userImage dbUser)
        Nothing
