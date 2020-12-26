module TreeContext where

-- We'll now extend contexts to work for trees. First, we'll set up a datatype
-- for plain trees. Trees will consist of Nodes, each with a piece of data of
-- type `a` and a list of trees which represent that Node's children.

data Tree a = Node a [Tree a]
  deriving (Eq)

data Context a =
    Empty
  | Loc a [Tree a] (Context a) [Tree a]

data TreeContext a = TC { getContext :: Context a, getItem :: Tree a }

-- A function to convert a regular tree into a context, with the initial
-- position set to the root.

contextOfTree :: Tree a -> TreeContext a
contextOfTree x =
  TC Empty x

-- A function to convert a context back into a regular tree.

contextToTree :: TreeContext a -> Tree a
contextToTree (TC (Empty) (treeArg)) = treeArg
contextToTree (TC (Loc parentData left parentContext right) (treeArg)) =
  contextToTree (TC (parentContext) (Node parentData ((reverse left) ++ [treeArg] ++ right)))

-- A function to get the subtree at the current position.

getCurTree :: TreeContext a -> Tree a
getCurTree (TC (Empty) (treeArg)) = treeArg
getCurTree (TC (Loc parentData left parentContext right) (treeArg)) = treeArg

moveLeftT :: TreeContext a -> TreeContext a
moveLeftT (TC Empty x) = TC Empty x
moveLeftT (TC (Loc parentData [] parentContext right) (treeArg)) =
  (TC (Loc parentData [] parentContext right) (treeArg))
moveLeftT (TC (Loc parentData left parentContext right) (treeArg)) =
  (TC (Loc parentData (getTail left) parentContext ([treeArg] ++ right)) (getHead left))

moveRightT :: TreeContext a -> TreeContext a
moveRightT (TC Empty x) = TC Empty x
moveRightT (TC (Loc parentData left parentContext []) (treeArg)) =
  (TC (Loc parentData left parentContext []) (treeArg))
moveRightT (TC (Loc parentData left parentContext right) (treeArg)) =
  (TC (Loc parentData ([treeArg] ++ left) parentContext (getTail right)) (getHead right))

moveUpT :: TreeContext a -> TreeContext a
moveUpT (TC (Empty) (treeArg)) = (TC (Empty) (treeArg))
moveUpT (TC (Loc parentData left parentContext right) (treeArg)) =
  (TC (parentContext) (Node parentData ((reverse left) ++ [treeArg] ++ right)))

moveDownT :: TreeContext a -> TreeContext a
moveDownT (TC (Empty) (Node cur (c:cs))) =
  (TC (Loc cur [] (Empty) cs) c)
moveDownT (TC (context) (Node cur [])) = (TC (context) (Node cur []))
moveDownT (TC (Loc parentData left parentContext right) (Node cur (c:cs))) =
  (TC (Loc cur [] (Loc parentData left parentContext right) cs) c)

getHead :: [a] -> a
getHead (x:xs) = x

getTail :: [a] -> [a]
getTail [] = []
getTail (x:xs) = xs

-- A function to replace the tree at the current position in the context
-- with a new subtree. Again, your function should never raise an error!

updateT :: Tree a -> TreeContext a -> TreeContext a
updateT new (TC Empty x) = TC Empty new
updateT new (TC (Loc parentData left parentContext right) (treeArg)) =
  (TC (Loc parentData left parentContext right) (new))

-- A function to insert a new subtree into the context. There are three
-- possible places to insert: left (as a new sibling before the current
-- position), right (as a new sibling after the current position), and down (as
-- a new first/left-most child of the current position). The cursor should end
-- up on the newly inserted item. If the insertion is invalid (inserting
-- left/right at the root), return the original context.

insertLeftT :: Tree a -> TreeContext a -> TreeContext a
insertLeftT _ (TC Empty x) = TC Empty x
insertLeftT new (TC (Loc parentData left parentContext right) (treeArg)) =
  moveLeftT (TC (Loc parentData ([new] ++ left) parentContext right) (treeArg))

insertRightT :: Tree a -> TreeContext a -> TreeContext a
insertRightT _ (TC Empty x) = TC Empty x
insertRightT new (TC (Loc parentData left parentContext right) (treeArg)) =
  moveRightT (TC (Loc parentData left parentContext ([new] ++ right)) (treeArg))

insertDownT :: Tree a -> TreeContext a -> TreeContext a
insertDownT _ (TC Empty x) = TC Empty x
insertDownT new (TC (Loc parentData left parentContext right) (Node cur x)) =
  moveDownT (TC (Loc parentData left parentContext right) (Node cur ([new] ++ x)))

-- A function to delete the whole subtree at the current position from the
-- context. The final position of the context should be (in decreasing priority)
-- the next sibling on the right, or the previous sibling on the left, or the
-- parent node if there are no other siblings.

deleteT :: TreeContext a -> TreeContext a
deleteT (TC Empty (Node x y)) = TC Empty (Node x [])
deleteT (TC (Loc parentData [] parentContext []) treeArg) =
  TC (parentContext) (Node parentData [])
deleteT (TC (Loc parentData left parentContext []) treeArg) =
  TC (Loc parentData (getTail left) parentContext []) (getHead left)
deleteT (TC (Loc parentData left parentContext right) treeArg) =
  TC (Loc parentData left parentContext (getTail right)) (getHead right)

-- If you would like to change the starting context that the main function of
-- this program, change the definition below.

initContext :: TreeContext Integer
initContext = contextOfTree initTree

initTree :: Tree Integer
initTree = Node 1 [Node 2 [], Node 3 []]
