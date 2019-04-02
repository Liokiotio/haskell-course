import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

ouu k | k == 3 = Just 4
      | otherwise = Nothing

instance MapLike ArrowMap where
    empty = ArrowMap (\empty -> Nothing)

    lookup z (ArrowMap f) = f z

    insert z a (ArrowMap f) = case f z of
        Nothing -> ArrowMap f
        Just x -> ArrowMap (\z -> Just a) 
    
    delete z (ArrowMap f) = case f z of
        Nothing -> ArrowMap f
        Just x -> ArrowMap (\z -> Nothing) 
    
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)