{-# LANGUAGE OverloadedStrings #-}
module Web.Simplebank
  ( Balances (..)
  , Card (..)
  , Configuration (..)
  , balances
  , card
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

data Card =
  Card
  { activationDate :: String -- TODO: thyme
  , cardStatus :: String
  , customerName :: String
  , expirationDate :: String -- TODO: thyme
  , fullStatus :: String -- TODO: sum type, if we can get all possible values...
  , groupStatus :: String -- TODO: sum type, if we can get all possible values...
  , indent :: String -- TODO: Why is this a string?
  } deriving Show

instance FromJSON Card where
    parseJSON (Object o) = Card <$>
                           o .: "activation_date" <*>
                           o .: "card_status" <*>
                           o .: "customer_name" <*>
                           o .: "expiration_date" <*>
                           o .: "full_status" <*>
                           o .: "group_status" <*>
                           o .: "indent"
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

-- {"full_status":"OPEN","group_status":"OPEN","card_status":"OPEN","indent":"XXXX","customer_name":"XXXX XXXX","activation_date":"XXX-XX-02T15:XX:XX+00:00","expiration_date":"XX/XX"}
card :: Session -> IO (Maybe Card)
card s = do
  r <- get s "https://bank.simple.com/card"
  return $ decode (r ^. responseBody)
