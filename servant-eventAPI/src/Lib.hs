{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Crypto.BCrypt
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Database.SQLite.Simple
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId :: Int,
    userEmail :: String,
    userPassword :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type GetUsers = "users" :> Get '[JSON] [User]

type CreateUser = "users" :> ReqBody '[JSON] User :> PostCreated '[JSON] UserCreationMsg

type API = GetUsers :<|> CreateUser

data UserCreationMsg = UserCreationMsg
  {message :: String}
  deriving (Generic, Show)

instance ToJSON UserCreationMsg

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

users :: [User]
users = []

server :: Server API
server = getUsers :<|> createUser
  where
    getUsers = return users
    createUser user = return (UserCreationMsg {message = "user successfully created"})
