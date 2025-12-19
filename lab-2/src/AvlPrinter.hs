module AvlPrinter (prettyPrint) where

import Avl 

prettyPrint :: Show a => AVL a -> IO ()
prettyPrint tree = putStrLn (drawTree tree)

-- Internal: build list of strings, then join
drawTree :: Show a => AVL a -> String
drawTree tree = unlines (draw tree)

draw :: Show a => AVL a -> [String]
draw Empty = ["(empty)"]
draw t@(Node h x l r) =
  let
    -- Show root
    rootStr = show x ++ " (h=" ++ show h ++ ", b=[" ++ show (balanceFactor t) ++ "]" ++ ")"

    -- Recursively draw left and right
    leftLines  = draw l
    rightLines = draw r

    -- Widths
    leftWidth  = if null leftLines  then 0 else maximum (map length leftLines)
    rightWidth = if null rightLines then 0 else maximum (map length rightLines)

    -- Pad lines to align
    padL = map (padRight leftWidth) leftLines
    padR = map (padRight rightWidth) rightLines
  in
    if null leftLines && null rightLines then
      [rootStr]
    else if null leftLines then
      rootStr : map ("└── " ++) padR
    else if null rightLines then
      rootStr : map ("├── " ++) padL
    else
      [rootStr] ++
      map ("├── " ++) padL ++
      map ("└── " ++) padR

-- Helper: pad string to length on the right
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '
