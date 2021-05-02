

--a
append l m = append_help l m [] where
    append_help [] m [] = m
    append_help [] m (a:acc) = append_help [] (a:m) acc
    append_help (lh:lt) m acc = append_help lt m (lh:acc)

--b
member l x = member l x False where
    member _ _ True = True
    member [] _ _ = False
    member (l:lt) x acc = member lt x (l==x)

--c
reverse' l = reverse_help l [] where
    reverse_help [] acc = acc
    reverse_help (l:lt) acc = reverse_help lt (l:acc)

--d
last' [x] = x
last' (l:lt) = last' lt

--e
delete' x l = delete_help x l [] where
    delete_help _ [] acc = reverse' acc
    delete_help x (l:lt) acc = delete_help x lt (if x==l then acc else (l:acc))

--f
pairing [] [] = []
pairing (l1:lt1) (l2:lt2)= (l1,l2):(pairing lt1 lt2)

--g 
split x l = split_help x l [[],[]] where
    split_help x [] [l1,l2] = [reverse' l1, reverse' l2]
    split_help x (l:lt) [l1,l2] = split_help x lt (if l < x then [l:l1,l2] else [l1,l:l2])

--h
map' f [] = []
map' f (l:lt) = (f l) : (map' f lt)
