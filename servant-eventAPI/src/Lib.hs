{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.Password.Bcrypt
import Data.Text
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

type GetUsers = "users" :> Get '[JSON] [String]

type CreateUser = "users" :> ReqBody '[JSON] User :> PostCreated '[JSON] UserCreationMsg

type API = GetUsers :<|> CreateUser

data UserCreationMsg = UserCreationMsg
  {message :: String}
  deriving (Generic, Show)

instance ToJSON UserCreationMsg

-- app :: Application
app :: Connection -> Application
app conn = serve api (server conn)

api :: Proxy API
api = Proxy

users :: [User]
users = []

-- Store in DB

server :: Connection -> Server API
server conn = getUsers :<|> createUser
  where
    getUsers :: Handler [String]
    getUsers = do
      let getUserQuery = "SELECT userEmail FROM User"
      result <- liftIO $ query_ conn getUserQuery
      let emails = Prelude.map fromOnly result
      return emails

    createUser user = do
      passwordHash <- hashPassword (mkPassword (Data.Text.pack (userPassword user)))
      let hashText = unPasswordHash passwordHash
      let query = "INSERT INTO User VALUES (?,?,?)"
      result <- liftIO $ execute conn query (userId user, Data.Text.pack (userEmail user), hashText)

      return
        ( UserCreationMsg
            { message = userEmail user ++ " successfully created!"
            }
        )

startApp :: IO ()
startApp = do
  conn <- open "servant-eventDB"
  -- run 8080 app (server conn)
  run 8080 (serve api (server conn))
