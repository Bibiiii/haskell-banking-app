{-|
Module      : DataTypes
Description : Data types related to the customer accounts.
* Customer
* Account
-}
{-# LANGUAGE DeriveGeneric #-}
module DataTypes
    (
      Customer (..),
      Account
    ) where

import GHC.Generics
import Control.Concurrent.STM

-- |The 'Account' type is a TVar of type 'Customer'
type Account = TVar Customer

-- | The Customer constructor creates a new record for a customer
-- | Customer is a derivation of the Show and Generic instances
data Customer = Customer {
            name :: String, -- ^ The customer name of type String
            number :: Int, -- ^ The account number of type Int
            balance :: Int -- ^ The account balance of type Int
        } deriving (Show, Generic)