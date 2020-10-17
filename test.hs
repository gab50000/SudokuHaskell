append :: a -> [a] -> [a]
append x list = x : list

(%>%) :: a -> (a -> b) -> b
a %>% f = f a
