{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.Except
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
--    ( (:>), (:<|>)((:<|>)), Get, JSON, Proxy(..), ServantErr, ServerT, serve )

-- type untuk 3 endpoint 
type MyAPI = "numba" :> Get '[JSON] Int
        :<|> "greet" :> Get '[JSON] String
        :<|> "greet" :> Capture "name" String :> Get '[JSON] String

api :: Proxy MyAPI
api = Proxy

-- handler untuk masing - masing endpoint
-- perhatikan urutan handler harus sesuai dengan type yang telah didefinisikan di atas!
myAPI :: ServerT MyAPI (ExceptT ServantErr IO)
myAPI = numbaH :<|> emptyGreetH :<|> greetWithNameH

numbaH :: ExceptT ServantErr IO Int
numbaH = return 42
 
emptyGreetH :: ExceptT ServantErr IO String
emptyGreetH = return "hi there!"
 
greetWithNameH :: String -> ExceptT ServantErr IO String
greetWithNameH n = return $ "hi there, " ++ n

-- jalankan web app di port 8000
--startApp :: IO () 
startApp = run 8000 (serve api myAPI)
