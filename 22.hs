 data Tree a = Node a [Tree a]

 --a
 sizeTree (Node _ []) = 1
 sizeTree (Node _ b) = 1 + maximum (map sizeTree b)

 --b
 sumTree (Node x []) = x
 sumTree (Node x b) = x + sum (map sumTree b)

 --c
 preTree (Node x []) = [x]
 preTree (Node x b) = x : (concat (map preTree b))


 mapTree f (Node x b) = Node (f x) (map (mapTree f) b)
 
 foldTree f (Node x b) = f x (map (foldTree f) b)


 --a
 sizeTree' = foldTree f where
     f _ [] = 1
     f _ b = 1 + maximum b

 --b
 sumTree' = foldTree (\x xt -> sum(x:xt))

 --c
 preTree' = foldTree (\x xt -> x:(concat xt))