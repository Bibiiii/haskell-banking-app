{-|
Module      : Main
Description : Main thread which creates ten “customers” (ten values of type Customer),
and spawns ten threads, one for each of these customers
-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import DataTypes
import AccountActions
import Util
import Control.Concurrent.ParallelIO 
import Data.Traversable

{- Main function
    main thread which creates ten “customers”
    (ten values of type Customer),
    and spawns ten threads, one for each of these customers
-}
main :: IO () -- ^ Performs an IO action
main = do
    -- Create 10 customers
    let accs = map createAccount [1..10]
    allAccounts <- sequence accs

    putStrLn "\n******************************************************"
    putStrLn "******************* All accounts: ********************"
    putStrLn "******************************************************"
    mapM_ showAccount allAccounts

    putStrLn "\n******************************************************"
    putStrLn "***************** Transferring funds *****************"
    putStrLn "******************************************************"
    
    -- replicateM_ 100 (forkIO $ transferRandom allAccounts) -- without parallelism
    parallel_ $ replicate 100 (forkIO $ transferRandom allAccounts) -- with parallelism

    threadDelay 100000 -- delay before showing all accounts again

    putStrLn "\n******************************************************"
    putStrLn "*********** All accounts after transfers: ************"
    putStrLn "******************************************************"
    mapM_ showAccount allAccounts