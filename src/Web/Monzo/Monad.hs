{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module Web.Monzo.Monad 
    ( MonzoM(..)
    , runMonzoM
    , listAccountsM
    , getBalanceM
    , listTransactionsM
    ) where

import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Web.Monzo

data Config = Config
    { configManager     :: Manager
    , configAccessToken :: AccessToken
    }

newtype MonzoM a = MonzoM 
    { unMonzoM :: ReaderT Config IO a 
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadIO
        )

-- | Evaluate MonzoM computation using Manager for HTTP connections and
-- AccessToken as the access token.
runMonzoM :: Manager -> AccessToken -> MonzoM a -> IO a
runMonzoM mgr token m = runReaderT (unMonzoM m) (Config mgr token)

-- | List all the accounts associated with an access token.
listAccountsM :: (MonadIO m, MonadReader Config m) => m (Either String AccountList)
listAccountsM = do
    conf <- ask
    liftIO $ listAccounts (configManager conf) (configAccessToken conf)

-- | Get the balance associated with an account ID.
getBalanceM :: (MonadIO m, MonadReader Config m) => AccountId -> m (Either String Balance)
getBalanceM acc = do
    conf <- ask
    liftIO $ getBalance (configManager conf) (configAccessToken conf) acc

-- | List the transactions associated with an account ID.
listTransactionsM :: (MonadIO m, MonadReader Config m) => AccountId -> m (Either String TransactionList)
listTransactionsM acc = do
    conf <- ask
    liftIO $ listTransactions (configManager conf) (configAccessToken conf) acc
