module Main where

import Test.Hspec
import BTree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "foldTree" $ do
        it "creates a balanced binary tree from a list of values" $ do
            let input1 = "ABCDEFGHIJ"
                expected1 = Node 3 (Node 2 (Node 1 (Node 0 Leaf 'A' Leaf) 'B' (Node 0 Leaf 'C' Leaf)) 'D' (Node 1 (Node 0 Leaf 'E' Leaf) 'F' (Node 0 Leaf 'G' Leaf))) 'H' (Node 1 (Node 0 Leaf 'I' Leaf) 'J' Leaf)
                input2 = [1, 2, 3, 4, 5]
                expected2 = Node 2 (Node 1 (Node 0 Leaf 1 Leaf) 2 (Node 0 Leaf 3 Leaf)) 4 (Node 1 (Node 0 Leaf 5 Leaf) 6 Leaf)
            foldTree input1 `shouldBe` expected1
            foldTree input2 `shouldBe` expected2