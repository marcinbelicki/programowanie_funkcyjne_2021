
odd' 0 = False
odd' 1 = True
odd' n = odd' (n - 1)

even' 0 = True
even' 1 = False 
even' n = even' (n - 1)
