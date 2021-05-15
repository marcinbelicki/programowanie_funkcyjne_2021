data BinTree a = Leaf a | Node a (BinTree a) (BinTree a)

--a
heightBinTree (Leaf _) = 1
heightBinTree (Node _ x y) = 1 + max (heightBinTree x) (heightBinTree y)

--b
sizeBinTree (Leaf _) = 1
sizeBinTree (Node _ x y) = 1 + sizeBinTree x + sizeBinTree y

--c
maxBinTree (Leaf x) = x
maxBinTree (Node x y z) = max x (max (maxBinTree y) (maxBinTree z))

--d
preBinTree (Leaf x) = [x]
preBinTree (Node x y z) = x:(concat[preBinTree y, preBinTree z])


mapBinTree f (Leaf x) = Leaf (f x)
mapBinTree f (Node x y z) = Node (f x) (mapBinTree f y) (mapBinTree f z)

foldBinTree f p (Leaf x) = f x p p
foldBinTree f p (Node x y z) = f x (foldBinTree f p y) (foldBinTree f p z)


--a
heightBinTree' = foldBinTree (\_ y z -> 1 + max y z) 0

--b
sizeBinTree' = foldBinTree (\_ y z -> 1 + y + z) 0

--c
maxBinTree' a = check (foldBinTree (\x y z -> max' (Just x) (max' y z)) Nothing a) where
    max' x Nothing = x
    max' Nothing y = y
    max' (Just x) (Just y) = Just (max x y)
    check (Just l) = l

--d
preBinTree' = foldBinTree (\x y z -> x:(concat[y,z])) [] where