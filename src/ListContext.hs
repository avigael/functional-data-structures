module ListContext where

-- A context takes a plain datastructure---a list, a tree, etc.---and adds an
-- extra bit of information describing a position within the datastructure. This
-- position behaves much like a cursor: we can move the cursor, lookup the value
-- at the cursor, or update/delete the value at the cursor.

-- We can represent a position in a non-empty plain list with three parts: the
-- list of elements before the position, the element at the position, and the
-- list of elements after the position. Otherwise, the context may be over an
-- empty list. The elements of the list will be of type `a`.

data ListContext a =
    LCEmpty
  | LCItems [a]   -- list of items before
            a     -- current item
            [a]   -- list of items after

-- A function to convert a regular list into a context, with the initial
-- position set to the first element (if the list is non-empty).

contextOfList :: [a] -> ListContext a
contextOfList [] = LCEmpty
contextOfList (x:xs) = LCItems [] x xs

-- A function to convert a context back into a regular list.

contextToList :: ListContext a -> [a]
contextToList (LCEmpty) = []
contextToList (LCItems x y z) = reverse x ++ [y] ++ z

-- A function to get the current item. Note that the function returns
-- `Maybe a`. Your function should return `Nothing` if the context is empty.

getCurItem :: ListContext a -> Maybe a
getCurItem (LCEmpty) = Nothing
getCurItem (LCItems x y z) = Just y

-- A functions to move the position left or right in the context. Since we
-- cannot actually change the input context in a pure functional language, this
-- function will return a context with the updated position. 
--
-- If the input context is empty, or the move is not valid (trying to move left
-- in the first position, or trying to move right in the last position), return
-- the original context with no change.

moveLeftL :: ListContext a -> ListContext a
moveLeftL (LCEmpty) = LCEmpty
moveLeftL (LCItems [] y z) = LCItems [] y z
moveLeftL (LCItems x y z) = LCItems (getTail x) (getHead x) ([y] ++ z)

moveRightL :: ListContext a -> ListContext a
moveRightL (LCEmpty) = LCEmpty
moveRightL (LCItems x y []) = LCItems x y []
moveRightL (LCItems x y z) = LCItems ([y] ++ x) (getHead z) (getTail z)

getHead :: [a] -> a
getHead (x:xs) = x

getTail :: [a] -> [a]
getTail [] = []
getTail (x:xs) = xs

-- A function to replace the item at the current position in the context
-- with a new element. If the input context is empty, return the input context
-- unchanged.
updateItem :: a -> ListContext a -> ListContext a
updateItem n (LCEmpty) = LCEmpty
updateItem n (LCItems x y z) = LCItems x n z

-- A function to insert a new element into the context *after* the current
-- position. The cursor should move to the new item after the insertion.
insertItem :: a -> ListContext a -> ListContext a
insertItem n (LCEmpty) = contextOfList [n]
insertItem n (LCItems x y z) = moveRightL (LCItems x y ([n] ++ z))

-- A function to delete the current element from the context. The cursor
-- should move to the item before the deleted position, or after the first
-- position when deleting the first item.
deleteItem :: ListContext a -> ListContext a
deleteItem (LCEmpty) = LCEmpty
deleteItem (LCItems [] y []) = LCEmpty
deleteItem (LCItems [] y (x:xs)) = LCItems [] x xs
deleteItem (LCItems (x:xs) y z) = LCItems xs x z

-- If you want to start from different context, change the definition of
-- "initContext" below.

initContext :: ListContext String
initContext = contextOfList []
