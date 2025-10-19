

sommeDeXaY:: Int->Int->Int 
sommeDeXaY x y | x==y = x 
               | x > y = 0 
               | otherwise = x + (sommeDeXaY (x+1) (y)) 


somme :: [Int] ->Int 
somme []= 0 
somme (x:xs) = x + somme xs 


last' :: [a]->a 
last' l = head (reverse l ) 

init' :: [a]-> [a]
init' l = take ((length l)-1) l 


select' :: [a] -> Int -> a 
select' (x:xs) 0 = x
select' (x:xs) n =  select' xs  (n-1) 

plusPlus :: [a] -> [a] ->[a] 
plusPlus [] l =l 
plusPlus l []= l 
plusPlus (x:xs) (l) = x : plusPlus xs l   

concat':: [[a]] -> [a] 
concat' []=[]
concat' (x:xs) = x ++ concat' xs 

map':: (a->b) -> [a] -> [b] 
map' f []=[]
map' f (x:xs) = f x : map' f xs 


longueur :: [a] ->Int 
longueur l = sum  ((map (\x->1) l)) 


func :: (a->a)->a->Int-> [a]
func f x 0 = [x] 
func f x n = [x]++func f (f x) (n-1)  

func' f x n = take n (iterate f x ) 






f1 :: Int -> [Int]
f1 n=  take n (func (+1) 0 n)
