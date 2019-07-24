{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Monzo
    ( Transaction(..)
    , TransactionList(..)
    , Balance(..)
    , Account(..)
    , AccountList(..)
    , AccessToken
    , AccountId

    , base
    , listAccounts
    , getBalance
    , listTransactions
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import GHC.Generics

import           Data.ByteString (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL

import           Data.Text (Text)
import qualified Data.Text as Text

import Data.Aeson

base :: String
base = "https://api.monzo.com"

type AccessToken = ByteString
type AccountId   = Text

get :: Manager -> String -> IO (Response BL.ByteString)
get mgr path = do
    req <- parseRequest (base <> path)
    httpLbs req mgr

authget :: Manager -> AccessToken -> String -> IO (Response BL.ByteString)
authget mgr token path = do
    req <- parseRequest (base <> path)
    flip httpLbs mgr req
        { requestHeaders = 
            [ ("Authorization", "Bearer " <> token)
            ]
        }

authgetjson :: FromJSON a => Manager -> AccessToken -> String -> IO (Either String a)
authgetjson mgr token path =
    eitherDecode' . responseBody <$> authget mgr token path

listAccounts :: Manager -> AccessToken -> IO (Either String AccountList)
listAccounts mgr token = authgetjson mgr token "/accounts"

getBalance :: Manager -> AccessToken -> AccountId -> IO (Either String Balance)
getBalance mgr token accId = authgetjson mgr token ("/balance?account_id=" <> Text.unpack accId)

listTransactions :: Manager -> AccessToken -> AccountId -> IO (Either String TransactionList)
listTransactions mgr token accId = authgetjson mgr token ("/transactions?account_id=" <> Text.unpack accId)

data TransactionList = TransactionList
    { tlTransactions :: [Transaction]
    }
    deriving (Generic, Show)

instance FromJSON TransactionList where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 2
        }

data Transaction = Transaction
    { transactionId             :: Text
    , transactionCreated        :: Text
    , transactionDescription    :: Text
    , transactionAmount         :: Integer
    , transactionCurrency       :: Text
    , transactionNotes          :: Text
    , transactionAccountBalance :: Integer
    , transactionCategory       :: Text
    , transactionIsLoad         :: Bool
    , transactionSettled        :: Text
    , transactionLocalAmount    :: Integer
    , transactionLocalCurrency  :: Text
    , transactionUpdated        :: Text
    , transactionAccountId      :: AccountId
    , transactionScheme         :: Text
    }
    deriving (Generic, Show)

instance FromJSON Transaction where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 11
        }

data Balance = Balance
    { balanceBalance :: Integer
    , balanceTotalBalance :: Integer
    , balanceBalanceIncludingFlexibleSavings :: Integer
    , balanceCurrency :: Text
    , balanceSpendToday :: Int
    , balanceLocalCurrency :: Text
    , balanceLocalExchangeRate :: Double
    } 
    deriving (Generic, Show)

instance FromJSON Balance where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 7
        }

data Owner = Owner
    { ownerUserId :: Text
    , ownerPreferredName :: Text
    , ownerPreferredFirstName :: Text
    }
    deriving (Generic, Show)

instance FromJSON Owner where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 5
        }

data Account = Account
    { accountId            :: AccountId
    , accountClosed        :: Bool
    , accountCreated       :: Text
    , accountDescription   :: Text
    , accountType          :: Text
    , accountCurrency      :: Text
    , accountCountryCode   :: Text
    , accountOwners        :: [Owner]
    , accountSortCode      :: Maybe Text
    , accountAccountNumber :: Maybe Text
    }
    deriving (Generic, Show)


instance FromJSON Account where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 7
        }

data AccountList = AccountList
    { alAccounts :: [Account]
    }
    deriving (Generic, Show)

instance FromJSON AccountList where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 2
        }

