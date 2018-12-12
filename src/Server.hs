{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Server where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Lazy (HashMap)
import Data.Proxy (Proxy(Proxy))
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class
import qualified Servant
import Servant hiding (Handler, runHandler, serve)
import Servant.Server (ServerT, err404)
import UnliftIO.Exception (throwIO)
import GHC.Generics
import qualified Data.HashMap.Lazy as HM
import Network.Wai (Application)

-- ========================================================================= --
-- API details

type MobyAPI
  = Lookup
  :<|> Healthcheck

mobyAPI :: Proxy MobyAPI
mobyAPI = Proxy

-- POST /lookup
--
-- lookup a synonym
type Lookup
  = "lookup"
    :> ReqBody '[JSON] LookupReqBody
    :> Verb 'POST 200 '[JSON] Synonyms

-- GET /health
--
-- lookup a synonym
type Healthcheck
  = "health" :> Verb 'GET 202 '[JSON] NoContent


data LookupReqBody = LookupReqBody
  { word :: Text
  } deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

data Synonyms = Synonyms
  { synonyms :: Text
  } deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

-- ------------------------------------------------------------------------- --

server :: ServerT MobyAPI Handler
server
  =    handleLookup
  :<|> handleHealthCheck

-- | POST /synonym
handleLookup :: LookupReqBody -> Handler Synonyms
handleLookup (LookupReqBody t) = do
  (Env hm) <- ask
  case HM.lookup t hm of
    Nothing -> throwIO err404
    Just syns -> pure $ Synonyms syns

-- | GET /health
handleHealthCheck :: Handler NoContent
handleHealthCheck = pure NoContent

-- ========================================================================= --

newtype Env = Env { getStorage :: HashMap Text Text }


newtype Handler a = Handler { unHandler :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)


runHandler :: Env -> Handler a -> IO a
runHandler env = flip runReaderT env . unHandler


mobyApp :: Env -> Application
mobyApp s =
  Servant.serve (Proxy @MobyAPI)  $
    Servant.hoistServer (Proxy @MobyAPI) (liftIO . runHandler s) server

