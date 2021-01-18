{-|
Module      : PrintData
Description : Functions for displaying account details
* draw
* helpers for draw
-}
module PrintData
    (
      draw
    ) where

import Text.PrettyPrint as PP
import DataTypes ( Customer (..) )

{- | The 'draw' function pretty-prints account details for a given Customer.
    It returns a layout 'Doc' (defined in PrettyPrint)
-}
draw :: Customer -> Doc -- ^ 'draw' takes one argument of type 'Customer'
draw = row
 where
    -- print a row
    row c = foldl1 (<|>) [ int (number c)
                         , text (name c)
                         , text ("£" ++ show (balance c))
                         ]

-- | The <|> function is a helper that puts a delimiter pipe between two docs
(<|>) :: Doc -> Doc -> Doc -- ^ '<|>' takes two arguments of type 'Doc'
x <|> y = x PP.<> text "\t|\t" PP.<> y