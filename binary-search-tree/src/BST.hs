module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Empty | Node (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node leftTree _ _) = Just leftTree

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ rightTree) = Just rightTree

bstValue :: BST a -> Maybe a
bstValue Empty =  Nothing
bstValue (Node _ value _) = Just value

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node leftTree value rightTree) = if x > value then Node leftTree value (insert x rightTree) 
     else Node (insert x leftTree) value rightTree

singleton :: a -> BST a
singleton x = Node Empty x Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node leftTree value rightTree) = toList leftTree ++ [value] ++ toList rightTree
