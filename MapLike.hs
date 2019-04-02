import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
con (ListMap y) (ListMap x) = ListMap (y ++ x)

instance MapLike ListMap where

    empty = ListMap []


    lookup z (ListMap ((k,v) : xs)) | z == k = Just v
                                    | otherwise = lookup z (ListMap xs)
    lookup z (ListMap []) = Nothing


    insert z a (ListMap []) = ListMap [(z,a)]
    insert z a (ListMap ((k,v) : xs)) | z == k = ListMap ((k,a) : xs)
                                      | otherwise = con (ListMap [(k,v)]) (insert z a (ListMap xs))

    delete z (ListMap []) = ListMap []
    delete z (ListMap ((k,v) : xs)) | z == k = ListMap xs
                                    | otherwise = con (ListMap [(k,v)]) (delete z (ListMap xs))