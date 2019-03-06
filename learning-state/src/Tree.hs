module Tree where

import Data.Traversable

newtype Endo b = Endo {appEndo :: b -> b}

instance Semigroup (Endo a) where
    Endo g <> Endo f = Endo (g . f)

instance Monoid (Endo a) where
    mempty = Endo id

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Functor Tree where
    fmap f Empty               = Empty
    fmap f (Leaf a)            = Leaf (f a)
    fmap f (Node left m right) = Node (fmap f left) (f m) (fmap f right)

instance Foldable Tree where
    -- foldMap f Empty = mempty
    -- foldMap f (Leaf a) = f a
    -- foldMap f (Node left m right) = foldMap f left <> f m <> foldMap f right
    foldMap = foldMapDefault

instance Traversable Tree where
    traverse f Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node left m right) = Node <$> traverse f left <*> f m <*> traverse f right


rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = traverse (\x -> if x < 0 then Nothing else Just x)


mapAccumL' :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL' f initial xs = undefined