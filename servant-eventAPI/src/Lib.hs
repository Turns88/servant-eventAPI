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
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Time
-- DATA TYPES
data NewUser = NewUser
  { newUserEmail :: String,
    newUserPassword :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewUser

data User = User
  { userId :: String,
    userEmail :: String,
    userPassword :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data NewEvent = NewEvent
{
  newEventTitle :: String,
  newEventDescription :: String,
  newEventLocation :: String,
  newEventTotalTickets :: Int,
  newEventImagePath :: String,
  newEventTime :: 
  
}

data Event = Event
  { eventName :: String,
    eventDescription :: String,
    eventLocation :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON Event

-- | The result of authentication/authorization
data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr
  deriving (Eq, Show, Read, Generic, Typeable, Functor)

-- | Datatype wrapping a function used to check authentication.
newtype BasicAuthCheck usr = BasicAuthCheck
  { unBasicAuthCheck :: BasicAuthData
                     -> IO (BasicAuthResult usr)
  }
  deriving (Generic, Typeable, Functor)

-- Routes
type GetUsers = 
  "users" 
  :> Get '[JSON] [String]

type CreateUser = 
  "users" 
  :> ReqBody '[JSON] NewUser 
  :> PostCreated '[JSON] UserCreationMsg


type CreateEvent= 
  "events" 
  :> BasicAuth "eventAPI" User
  :> ReqBody '[JSON] NewEvent 
  :> PostCreated '[JSON] UserCreationMsg

type API = GetUsers :<|> CreateUser :<|>   :> BasicAuth "eventAPI" User
:> CreateEvent

data UserCreationMsg = UserCreationMsg
  {message :: String}
  deriving (Generic, Show)

instance ToJSON UserCreationMsg

authCheck :: Connection -> AuthCheck User
authCheck conn = do
  BasicAuthCheck $ \basicAuthData ->
    let username = decodeUtf8 (basicAuthUsername basicAuthData)
      password = decodeUtf8 (basicAuthPassword basicAuthData)
      passwordHash <- hashPassword (mkPassword (Data.Text.pack (password)))
      let hashText = unPasswordHash passwordHash
      result <- query conn "SELECT email, pwHash FROM users WHERE email = ?" (username)
      case result of
        [(email, hash)] ->
          if passwordHash == hash
          then return (Authorized (User email))
          else return Unauthorized
        [] -> return Unauthorized  
        _  -> error "Multiple users found with same email!" 

-- app :: Application
app :: Connection -> Application
app conn = serve api (server conn)

api :: Proxy API
api = Proxy

-- Store in DB

server :: Connection -> Server API
server conn = getUsers :<|> createUser :<|> createEvent
  where
    getUsers :: Handler [String]
    getUsers = do
      let getUserQuery = "SELECT userEmail FROM User"
      result <- liftIO $ query_ conn getUserQuery
      let emails = Prelude.map fromOnly result
      return emails

    createUser user = do
      -- generate uuid
      newUUID <- liftIO $ nextRandom
      passwordHash <- hashPassword (mkPassword (Data.Text.pack (newUserPassword user)))
      let hashText = unPasswordHash passwordHash
      let query = "INSERT INTO user VALUES (?,?,?)"
      result <- liftIO $ execute conn query (UUID.toText newUUID, Data.Text.pack (newUserEmail user), hashText)

      return
        ( UserCreationMsg
            { message = newUserEmail user ++ " successfully created!"
            }
        )
basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

startApp :: IO ()
startApp = do
  conn <- open "servant-eventDB"
  -- run 8080 app (server conn)
  --run 8080 (serve api (server conn))
   run 8080 (serveWithContext api basicAuthServerContext (server conn))
