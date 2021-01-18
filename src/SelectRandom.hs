{-|
Module      : SelectRandom
Description : Fuctions to select random items of a given type
* selectRandomNumber
* selectRandomName
* selectRandomAccount
-}
module SelectRandom
    (
      selectRandomName,
      selectRandomNumber,
      selectRandomAccount
    ) where

import System.Random
import DataTypes(Account)

-- | The 'selectRandomNumber' function returns an IO action with a random number in a given range
selectRandomNumber :: Int -> Int -> IO Int -- ^ 'selectRandomNumber' takes two arguments of type 'Int'
selectRandomNumber num1 num2 = randomRIO (num1, num2) :: IO Int

-- | The 'selectRandomName' function returns a random name from a list of Strings
selectRandomName :: [String] -> IO String -- ^ 'selectRandomName' takes one argument: a list of type 'String'
selectRandomName nameList = do
    let len = length nameList
    randomNumber <- selectRandomNumber 1 (length nameList - 1)
    let randomName = nameList !! randomNumber
    return randomName

-- | The 'selectRandomAccount' function returns a random account from a list of Accounts
selectRandomAccount :: [IO Account] -> IO Account -- ^ 'selectRandomAccount' takes one argument: a list of type 'IO Account'
selectRandomAccount accounts = do
    rn1 <- selectRandomNumber 0 10
    rn2 <- selectRandomNumber 0 10

    let randomAccount = accounts !! rn1
    if rn1 == rn2 then selectRandomAccount accounts else randomAccount
    