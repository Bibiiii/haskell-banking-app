{-|
Module      : Util
Description : Universal utils
* txtToList
-}
module Util
    ( txtToList
    ) where        

-- | The 'txtToList' function takes a file and returns a list of the items in it
txtToList :: FilePath -> IO [String]-- ^ 'txtToList' takes one argument: the filepath of the file
txtToList filename = do
    content <- readFile filename
    return $ lines content