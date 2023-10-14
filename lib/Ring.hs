module Ring where

{- | Ring which can be rotated forwards and backwards.
Maybe not a real "ring"?
But name seems to fit.

This is pretty crappy and combines the worst of all worlds.
A simple mod-wrapped int would perform the task better than this.
-}
data Ring a = Ring [a] a [a]
            | EmptyRing
    deriving (Show)

ringFromList :: [a] -> Ring a
ringFromList [] = EmptyRing
ringFromList xs = Ring (reverse $ tail xs) (head xs) (tail xs)

ringToList :: Ring a -> [a]
ringToList (Ring as x bs) = x : bs ++ reverse as
ringToList EmptyRing = []

ringFocus :: Ring a -> Maybe a
ringFocus (Ring _ x _) = Just x
ringFocus EmptyRing = Nothing

ringRotate :: Ring a -> Ring a
ringRotate r@(Ring _ _ []) = r
ringRotate (Ring as x bs) = Ring (x : init as) (head bs) (tail bs ++ [x])
ringRotate EmptyRing = EmptyRing

ringRotateBack :: Ring a -> Ring a
ringRotateBack r@(Ring [] _ _) = r
ringRotateBack (Ring as x bs) = Ring (tail as ++ [x]) (head as) (x : init bs)
ringRotateBack EmptyRing = EmptyRing

ringFilter :: (a -> Bool) -> Ring a -> Ring a
ringFilter f = ringFromList . filter f . ringToList

-- [1] 2 [1]
-- [1] _ [1]
-- [] 1 []

-- invariants
-- each element occurs at most once
