module AppendList where
    
-- In the functional programming style, we usually avoid mutating or updating
-- memory cells, preferring instead pure operations.

-- The elements of the list will be of type `a`.
newtype AppendList a = AList ([a] -> [a])

-- The empty list is represented using the identity function, by
-- AList (\x -> x)
empty :: AppendList a
empty = AList (\x -> x)

-- A function that takes on argument and returns the AppendList that corresponds
-- to the list with just that argument as element
singleton :: a -> AppendList a
singleton y = AList (\x -> y : x)

-- A function that converts an AppendList to the regular list that it
-- represents
toList :: AppendList a -> [a]
toList (AList f) = f []

-- A function that prepends an element to any AppendList, just like cons
-- (written : ) does for lists
alistCons :: a -> AppendList a -> AppendList a
alistCons a (AList f) = AList ((\x -> a : x) . f)

-- A replication function which makes  an AppendList by repeating the given
-- element for a given number of times (possibly zero times).
alistReplicate :: Int -> a -> AppendList a
alistReplicate n f
  | (n == 0) = empty
  | otherwise = alistCons f (alistReplicate (n - 1) f)

-- A function to append two AppendLists together.
alistAppend :: AppendList a -> AppendList a -> AppendList a
alistAppend (AList m) (AList n) = AList (m . n)

-- A concatenation function which takes a list of AppendLists and turns
-- them into a single AppendList (preserving the order).
alistConcat :: [AppendList a] -> AppendList a
alistConcat [] = empty
alistConcat (x:xs) = alistAppend x (alistConcat xs)

-- A function that does the opposite, converting a normal list into an
-- AppendList
fromList :: [a] -> AppendList a
fromList [] = empty
fromList (x:xs) = alistCons x (fromList xs)

-- A function that computes the head of an AppendList (your function may
-- fail when the AppendList is empty).
alistHead :: AppendList a -> a
alistHead x = getHead (toList x)

getHead :: [a] -> a
getHead (x:xs) = x

-- A foldr and map functions for AppendLists, just like for Lists
alistFoldr :: (a -> b -> b) -> b -> AppendList a -> b
alistFoldr x y z = foldr x y (toList z)

alistMap :: (a -> b) -> AppendList a -> AppendList b
alistMap x y = fromList (map x (toList y))

-- A a function that computes the tail of an AppendList.
alistTail :: AppendList a -> AppendList a
alistTail x = fromList (getTail (toList x))

getTail :: [a] -> [a]
getTail (x:xs) = xs

-- Basic Tests
checkEq :: (Eq a, Show a) => a -> a -> String
checkEq expected got
    | expected == got = "PASS"
    | otherwise       = "FAIL Expected: " ++ show expected ++ " Got: " ++ show got

tests :: IO ()
tests = let myList  = [1, 3, 5, 7, 9] :: [Int]
            myList' = [8, 6, 4, 2, 0] :: [Int] in
          do putStr "toList/fromList: "
             putStrLn $ checkEq myList (toList . fromList $ myList)

             putStr "alistHead: "
             putStrLn $ checkEq (head myList') (alistHead . fromList $ myList')

             putStr "alistTail: "
             putStrLn $ checkEq (tail myList) (toList . alistTail . fromList $ myList)

             putStr "alistFoldr: "
             putStrLn $ checkEq (sum myList) (alistFoldr (+) 0 $ fromList myList)

             putStr "alistMap: "
             putStrLn $ checkEq (map (+ 1) myList') (toList (alistMap (+ 1) $ fromList myList'))

             putStr "alistAppend: "
             putStrLn $ checkEq (myList ++ myList') (toList $ alistAppend (fromList myList) (fromList myList'))

             putStr "alistConcat: "
             putStrLn $ checkEq (myList' ++ myList) (toList $ alistConcat [fromList myList', fromList myList])

             putStr "alistReplicate: "
             putStrLn $ checkEq ([42, 42, 42, 42, 42] :: [Int]) $ toList (alistReplicate 5 42)
