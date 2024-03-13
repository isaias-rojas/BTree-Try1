module BTree
    ( Tree(..)
    , foldTree
    ) where

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h


foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 1 Leaf x Leaf
insert x (Node _ left y right)
    | hl < hr   = let newLeft = insert x left
                      newHl = height newLeft
                  in balance (Node (1 + max newHl hr) newLeft y right)
    | otherwise = let newRight = insert x right
                      newHr = height newRight
                  in balance (Node (1 + max hl newHr) left y newRight)
    where
        hl = height left
        hr = height right

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance (Node _ left x right)
    | hl > hr + 1 = balanceLeft left x right
    | hr > hl + 1 = balanceRight left x right
    | otherwise   = Node (1 + max hl hr) left x right
    where
        hl = height left
        hr = height right

balanceLeft :: Tree a -> a -> Tree a -> Tree a
balanceLeft (Node _ ll lx lr@(Node lrh _ _ _)) x r
    | height ll >= lrh = Node (1 + max lrh (height r)) (Node (1 + height ll) ll lx lr) x r
    | otherwise        = Node (1 + max (height ll) (height r)) ll lx (Node (1 + max lrh (height r)) lr x r)
balanceLeft ll x r = Node (1 + max (height ll) (height r)) (Node (1 + height ll) ll x Leaf) Leaf r

balanceRight :: Tree a -> a -> Tree a -> Tree a
balanceRight l x (Node _ rl rx rr@(Node rrh _ _ _))
    | rrh >= height rl = Node (1 + max (height l) rrh) l x (Node (1 + rrh) rl rx rr)
    | otherwise        = Node (1 + max (height l) (height rl)) (Node (1 + max (height l) (height rl)) l x rl) rx rr
balanceRight l x rr = Node (1 + max (height l) (height rr)) l x (Node (1 + height rr) Leaf Leaf rr)