{-# LANGUAGE OverloadedStrings #-}
module Web.Simplebank
  ( Balances (..)
  , Configuration (..)
  , balances
  , getSession
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import Network.Wreq (FormParam( (:=)), responseBody)
import Network.Wreq.Session

data Configuration =
  Configuration
  {
    username :: String
  , password :: String
  }

-- {"total":XXXXX,"safe_to_spend":XXXXX,"bills":X,"deposits":X,"pending":X,"goals":XXXXX}
data Balances =
  Balances
  { bills       :: Int
  , deposits    :: Int
  , goals       :: Int
  , pending     :: Int
  , safeToSpend :: Int
  , total       :: Int
  } deriving Show

instance FromJSON Balances where
    parseJSON (Object o) = Balances <$>
                           o .: "bills" <*>
                           o .: "deposits" <*>
                           o .: "goals" <*>
                           o .: "pending" <*>
                           o .: "safe_to_spend" <*>
                           o .: "total"
    parseJSON _          = mzero

-- | Generate an authenticated session which can be used to access other API
-- endpoints.
getSession :: Configuration -> IO Session
getSession (Configuration u p) =
   withSession $ \s -> do
     _ <- post s "https://bank.simple.com/signin" [ "username" := u
                                                  , "password" := p
                                                  ]
     return s

balances :: Session -> IO (Maybe Balances)
balances s = do
  r <- get s "https://bank.simple.com/account/balances"
  return $ decode (r ^. responseBody)
