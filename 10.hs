
--a
map2 f [] [] = []
map2 f (l1:lt1) (l2:lt2) = (f l1 l2) : (map2 f lt1 lt2)

--b
filter' p [] = []
filter' p (l:lt) = if p l then l:(filter' p lt) else filter' p lt
