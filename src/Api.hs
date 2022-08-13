{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
(app) where

--import Api.User (UsersAPI, userApi, userServer)
import Api.Users
import Config (AppT (..), Config (..))
--import Control.Monad.Reader (MonadIO, runReaderT)
import Servant.Auth.Server
--import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy)
import Servant
--  ( Proxy (Proxy),
--    Raw,
--    Server,
--    serve,
--    serveDirectoryFileServer,
--    (:<|>) ((:<|>)),
--    serveWithContext,
--
--  )
--import Servant.Server
--  ( Application,
--    Handler (Handler),
--    hoistServer,
--    HasServer (ServerT)
--  )

-- | This is the function we export to run our 'UserAPI'. Given
-- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- can run.
--userApp :: Config -> Application
--userApp cfg = cors (const $ Just simpleCorsResourcePolicy) $ serve adminLoginApi (appToServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appHandler :: MonadIO m => ServerT (AppAPI auth) (AppT m)
appHandler = usersHandler :<|> files

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: MonadIO m => ServerT Raw (AppT m)
files = serveDirectoryFileServer "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type AppAPI auth = UsersApi auth :<|> Raw

appApi :: Proxy (AppAPI '[JWT])
appApi = Proxy

-- | Finally, this function takes a configuration and runs our 'UserAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> Config -> Application
app ctx cs jwts cfg =
  serveWithContext appApi ctx $
    hoistServerWithContext appApi (Proxy :: Proxy '[CookieSettings, JWTSettings]) (convertApp cfg ) appHandler
