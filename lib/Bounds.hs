module Bounds where

data Bounds = Bounds
    { boundsL :: Integer
    , boundsR :: Integer
    , boundsT :: Integer
    , boundsB :: Integer
    }
    deriving (Eq, Show)

boundsAdd :: Bounds -> (Integer, Integer) -> Bounds
boundsAdd (Bounds l r t b) (x, y) = Bounds (l + x) (r + x) (t + y) (b + y)

boundsAdd4 :: Bounds -> (Integer, Integer, Integer, Integer) -> Bounds
boundsAdd4 (Bounds l r t b) (dl, dr, dt, db) =
    Bounds (l + dl) (r + dr) (t + dt) (b + db)

mul4 ::
    (Integer, Integer) ->
    (Integer, Integer, Integer, Integer) ->
    (Integer, Integer, Integer, Integer)
mul4 (x, y) (l, r, t, b) = (x * l, x * r, y * t, y * b)

boundsCentre :: Bounds -> (Integer, Integer)
boundsCentre (Bounds l r t b) =
    ( (r + l) `quot` 2
    , (b + t) `quot` 2
    )

boundsContains :: (Integer, Integer) -> Bounds -> Bool
boundsContains (x, y) (Bounds l r t b) =
    x >= l && x <= r && y >= t && y <= b

boundsContainsBounds :: Bounds -> Bounds -> Bool
boundsContainsBounds (Bounds l r t b) bs =
    boundsContains (l, t) bs && boundsContains (r, b) bs
