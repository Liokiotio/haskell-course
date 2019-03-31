data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord x y) = Coord (a x + w/2) (a y + w/2) where
  a z = fromIntegral z * w

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = Coord (floor $ x/w) (floor $ y/w)