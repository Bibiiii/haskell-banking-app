{-|
Module      : AccountActions
Description : Actions related to customer accounts
* createAccount
* showAccount
* transferRandom
* credit
* debit
* transfer
-}
module AccountActions
    (
      createAccount, createCustomer, showAccount, transferRandom, credit, debit, transfer
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import SelectRandom ( selectRandomName, selectRandomNumber, selectRandomAccount )
import DataTypes ( Customer (..), Account )
import Util
import Text.Printf
import Text.PrettyPrint
import System.Random ( Random(randomRIO) )
import PrintData
import Control.Concurrent.ParallelIO 
import GHC.Conc.Sync

{- | The 'createCustomer' function creates a Customer account.
    It returns a new Customer record with a random name, the given account number and account balance.
-}
createCustomer :: Int -> IO Customer -- ^  'createCustomer' takes one argument of type 'Int'
createCustomer number = do
    randomName <- selectRandomName =<< txtToList "randomNames.txt" -- get random name from list
    return Customer {
      name = randomName,
      number = 1000 + number, -- for realistic-looking account number, add 1000
      balance = 1000
    }

{- | The 'createAccount' function saves a new Customer Account.
    It returns a 'newTVarIO' of the saved account, in 'Customer' format.
-}
createAccount :: Int -> IO Account
createAccount number = newTVarIO =<< createCustomer number

{- | The 'showAccount' function displays the account in a table format
    It returns an empty IO action
-}
showAccount :: Account -> IO () -- ^  'showAccount' takes one argument of type 'Account'
showAccount acc = do
    a <- readTVarIO acc
    putStrLn $ render (draw a)

{- | The 'credit' function reads the balance of an account and credits it with the given amount.
    It writes the new balance to the Account TVar.
    It returns an empty STM action
-}
credit :: Int -> Account -> STM () -- ^  'credit' takes two arguments of type 'Int' and 'Account'
credit amount account = do
    acc <- readTVar account
    let b = balance acc
    writeTVar account (Customer (name acc) (number acc) (b + amount))

{- | The 'debit' function reads the balance of an account and debits the specified amount from the account.
    It writes the new balance to the Account TVar.
    It returns an empty STM action
-}
debit :: Int -> Account -> STM () -- ^  'debit' takes two arguments of type 'Int' and 'Account'
debit amount account = do
    acc <- readTVar account
    let b = balance acc
    writeTVar account (Customer (name acc) (number acc) (b - amount))

{- | The 'transfer' function debits one customer account, and credits the amount to another customer account.
    It makes use of the 'credit' and 'debit' functions.
    It uses 'atomically' to ensure that actions are isolated.
    It returns an empty IO action
-}
transfer :: Int -> Account -> Account -> STM () -- ^  'transfer' takes three arguments of type 'Int', 'Account' and 'Account'
transfer amount from to = do
    f <- readTVar from
    t <- readTVar to

    if balance f >= amount
        then do
            debit amount from
            credit amount to
        else do
            retry

{- | The 'transferRandom' function selects two random accounts and a random amount to transfer between them.
    It performs a random delay after the action.
    It returns an empty IO action
-}
transferRandom :: [Account] -> IO () -- ^  'transferRandom' takes one argument: a list of type 'Account'
transferRandom accounts = do
    rn1 <- randomRIO (0, length accounts - 1)
    rn2 <- randomRIO (0, length accounts - 1)
    let randomAcc1 = accounts !! rn1
    let randomAcc2 = accounts !! rn2

    randomAmount <- randomRIO(10,50)

    ra1 <- readTVarIO randomAcc1
    ra2 <- readTVarIO randomAcc1

    printf "Attempting to transfer £%d from #%d to #%d...\n" randomAmount (number ra1) (number ra2)
    atomically $ transfer randomAmount randomAcc1 randomAcc2

    randomDelay <- randomRIO (100, 100000) :: IO Int
    
    threadDelay randomDelay