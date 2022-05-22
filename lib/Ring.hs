module Ring where

{- | Ring which can be rotated forwards and backwards.
Maybe not a real "ring"?
But name seems to fit.

This is pretty crappy and combines the worst of all worlds.
A simple mod-wrapped int would perform the task better than this.
-}
data Ring a = Ring [a] a [a]
    deriving (Show)

ringFromList :: [a] -> Ring a
ringFromList [] = undefined
ringFromList xs = Ring (reverse $ tail xs) (head xs) (tail xs)

ringFocus :: Ring a -> a
ringFocus (Ring _ x _) = x

ringRotate :: Ring a -> Ring a
ringRotate r@(Ring [] _ []) = r
ringRotate (Ring as x bs) = Ring (x : init as) (head bs) (tail bs ++ [x])

ringRotateBack :: Ring a -> Ring a
ringRotateBack r@(Ring [] _ []) = r
ringRotateBack (Ring as x bs) = Ring (tail as ++ [x]) (head as) (x : init bs)
