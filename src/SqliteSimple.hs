{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module SqliteSimple where

import Control.Applicative ()
import Database.SQLite.Simple (Connection (..), FromRow (..), NamedParam (..), Only (..),
                               ResultError (..), SQLData (..), close, execute, executeNamed,
                               execute_, field, open, queryNamed, query_)
import Database.SQLite.Simple.FromField (Field (..), FromField (..), returnError)
import Database.SQLite.Simple.FromRow (FromRow (..))
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))

import qualified Data.Text as T
import Database.SQLite.Simple.QQ (sql)


data PhoneType
    = HomeNumber
    | WorkNumber
    deriving (Eq, Show, Read)

phoneTypeToText :: PhoneType -> T.Text
phoneTypeToText = \case
    HomeNumber -> "home"
    WorkNumber -> "work"

instance ToField PhoneType where
    toField = SQLText . phoneTypeToText

instance FromField PhoneType where
    fromField (Field (SQLText "home") _) = Ok HomeNumber
    fromField (Field (SQLText "work") _) = Ok WorkNumber
    fromField f = returnError ConversionFailed f "need 'HomeNumber' or 'WorkNumber'"

data Person = Person
    { pUUID      :: !Integer
    , pName      :: !T.Text
    , pEmail     :: !T.Text
    , pPhoneType :: !PhoneType
    , pNumber    :: !T.Text
    } deriving stock (Eq, Show, Read)

instance FromRow Person where
    fromRow = Person <$> field <*> field <*> field <*> field <*> field

-- when inserting a new Phone, ignore phoneId. SQLite will provide it for us.
instance ToRow Person where
    toRow (Person _pUUID pName pEmail pPhoneType pNumber) = toRow (pName, pEmail, pPhoneType, pNumber)

createPersonTable :: Connection -> IO ()
createPersonTable conn = execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS person
        ( id INTEGER PRIMARY KEY AUTOINCREMENT
        , name TEXT NOT NULL
        , email TEXT UNIQUE NOT NULL
        , phone_number TEXT UNIQUE NOT NULL
        , phone_type TEXT NOT NULL
        )
    |]

createNewPerson :: Connection -> IO ()
createNewPerson conn = executeNamed conn [sql|
    INSERT INTO person
        (name, email, phone_number, phone_type)
    VALUES
        (:name, :email, :phoneNumber, :phoneType)
    |] [ ":name"        := ("Justina" :: T.Text)
       , ":phoneNumber" := ("+91234567899" :: T.Text)
       , ":email"       := ("justina@wildnout.com" :: T.Text)
       , ":phoneType"   := WorkNumber
       --- ^ This would've been meaningless if there was no ToField instance defined for PhoneType
       ]

getAllPerson :: Connection -> IO [Person]
getAllPerson conn = query_ conn [sql|
    SELECT id
         , name
         , email
         , phone_type
         , phone_number
      FROM person
|]

-- Deletes all the data in person table to give us a fresh start
flushPersonTable :: Connection -> IO ()
flushPersonTable conn =
    execute_ conn [sql|
        DROP TABLE IF EXISTS person;
    |]

resetDb :: Connection -> IO ()
resetDb conn = do
    flushPersonTable conn
    createPersonTable conn

main :: IO ()
main = do
  conn <- open "test.db"
  resetDb conn
  createNewPerson conn
  rows <- getAllPerson conn
  mapM_ print rows
  close conn
