{-# LANGUAGE ScopedTypeVariables #-}
module Client
  ( Client.lookup
  , Client.healthCheck
  ) where

import Data.Either
import Data.Text
import Data.Functor
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API hiding (header)
import Servant.Client

import Server hiding (word)


lookup_ :<|> healthCheck_ = client mobyAPI

globalEnv :: IO ClientEnv
globalEnv =
  flip mkClientEnv (BaseUrl Http "localhost" 3000 "") <$> newManager defaultManagerSettings

-- ========================================================================= --

lookup_ :: LookupReqBody -> ClientM Synonyms

lookup :: Text -> IO (Either ServantError Text)
lookup w = globalEnv >>= (fmap.fmap) synonyms . runClientM (lookup_ (LookupReqBody w))

-------------------------------------------------------------------------------

healthCheck_ :: ClientM NoContent

healthCheck :: IO (Either ServantError ())
healthCheck = globalEnv >>= (fmap . fmap) (const ()) . runClientM healthCheck_


