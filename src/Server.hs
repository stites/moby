{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Data.Text
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Lazy (HashMap)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class
import Servant hiding (Handler)
import Servant.Server (ServerT, err404)
import UnliftIO.Exception (throwIO)
import GHC.Generics
import qualified Data.HashMap.Lazy as HM

-- ========================================================================= --
-- API details

type MobyAPI
  = Lookup
  :<|> Healthcheck

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


newtype LookupReqBody = LookupReqBody
  { word :: Text
  } deriving stock (Generic)
    deriving newtype (FromJSON, ToJSON)

newtype Synonyms = Synonyms
  { synonyms :: Text
  } deriving stock (Generic)
    deriving newtype (FromJSON, ToJSON)

-- ========================================================================= --
newtype Env = Env { getStorage :: HashMap Text Text }

newtype Handler a = Handler { unHandler :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

server :: ServerT MobyAPI Handler
server = handleLookup
   :<|> handleHealthCheck

-- | POST /synonym
handleLookup :: LookupReqBody -> Handler Synonyms
handleLookup (LookupReqBody t) = do
  (Env hm) <- ask
  case HM.lookup t hm of
    Nothing -> throwIO err404
    Just syns -> pure $ Synonyms syns


handleHealthCheck :: Handler NoContent
handleHealthCheck = pure NoContent
