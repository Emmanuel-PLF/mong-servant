{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Config (AppT (..), Config (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

--import Control.Monad.Metrics (increment, metricsCounters)
--import qualified Control.Monad.Metrics as Metrics
import Data.Aeson.Encode.Pretty (encodePretty)

--import Servant.JS (vanillaJS, writeJSForAPI)

import Data.ByteString.Lazy.Char8 qualified as BL8

--import Data.HashMap.Lazy (HashMap)
--import Data.IORef (readIORef)
--import Data.Int (Int64)
import Data.OpenApi (
    HasDescription (description),
    HasEmail (email),
    HasInfo (info),
    HasLicense (license),
    HasName (name),
    HasPassword (password),
    HasServers (servers),
    HasTitle (title),
    HasUrl (url),
    HasVersion (version),
    OpenApi,
    URL (URL),
 )
import Data.Text (Text)
import Database (User, runDb, userBio, userEmail, userImage, userName)
import Database qualified as Md
import Database.Persist.MongoDB (
    Entity (Entity),
    PersistQueryRead (selectFirst),
    selectList,
    (==.),
 )
import Lens.Micro ((&), (.~), (?~), (^.))
import Metrics.Metrics as M
import Query.User (insertUser)
import Servant (
    Capture,
    Get,
    HasServer (ServerT),
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    err404,
    throwError,
    type (:<|>) (..),
    type (:>),
 )
import Servant.OpenApi
import System.Metrics
import System.Metrics.Counter qualified as C
import Types.BCrypt (hashPassword)
import Types.User (
    UserAddress (..),
    UserRegister (..),
    UserResponse (UserResponse),
    UserResponseBis (UserResponseBis),
 )

type UserAPI =
    "users" :> Get '[JSON] [UserResponse]
        :<|> "users"
            :> Capture "name" Text
            :> Get '[JSON] UserResponseBis
        :<|> "register"
            :> ReqBody '[JSON] UserRegister
            :> Post '[JSON] UserResponse

--    :<|> "metrics"
--      :> Get '[JSON] (HashMap Text Int64)

withHandle :: Store AllMetrics -> (M.Handle -> IO a) -> IO a
withHandle s f = do
    auc <- createCounter (Metric @"perservant.allusers") () s
    suc <- createCounter (Metric @"perservant.singleuser") () s
    cuc <- createCounter (Metric @"perservant.createuser") () s
    f $ M.Handle suc auc cuc

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser -- :<|> waiMetrics

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [UserResponse]
allUsers = do
    increment hAllUsersC
    logDebugNS "web" "allUsers"
    au <- runDb (selectList [] [])
    mapM mkUserResponse au

increment :: MonadIO m => (Handle -> C.Counter) -> AppT m ()
increment f = do
    h <- asks hMetrics
    liftIO $ C.inc $ f h

-- | Returns a user by name or throws a 404 error.
singleUser :: MonadIO m => Text -> AppT m UserResponseBis
singleUser str = do
    increment hSingleUserC
    logDebugNS "web" "singleUser"
    maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
    case maybeUser of
        Nothing ->
            throwError err404
        Just (Entity personId person) ->
            return
                ( UserResponseBis
                    (userName person)
                    (UserAddress "5023" "la mutte" 49740)
                )

mkUserResponse :: (MonadIO m) => Entity User -> AppT m UserResponse
mkUserResponse (Entity pId per) = do
    return
        ( UserResponse
            (userEmail per)
            (userName per)
            (userBio per)
            (userImage per)
        )

-- | Creates a user in the database.
createUser :: MonadIO m => UserRegister -> AppT m UserResponse
createUser userReg = do
    increment hCreateUserC
    logDebugNS "web" "creating a user"
    hashedPw <- hashPassword $ userReg ^. password
    newUser <- runDb $ insertUser (userReg ^. name) (userReg ^. email) (userReg ^. bio) hashedPw
    logDebugNS "web" "after create user"
    mkUserResponse newUser

{- | Return wai metrics as JSON
 waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
 waiMetrics = do
 increment "metrics"
 logDebugNS "web" "metrics"
 metr <- Metrics.getMetrics
 liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)
-}

{- | Generates JavaScript to query the User API.
 generateJavaScript :: IO ()
 generateJavaScript =
  writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"
-}

-- | Swagger spec for Todo API.
userSwagger :: OpenApi
userSwagger =
    toOpenApi userApi
        & info . title .~ "User API"
        & info . version .~ "1.0"
        & info . description ?~ "This is an API that tests swagger integration"
        & info . license ?~ ("EPU" & url ?~ URL "http://...")
        & servers .~ ["http://localhost:8081" & description ?~ "super server test"]

writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty userSwagger)
