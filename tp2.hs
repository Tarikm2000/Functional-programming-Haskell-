alterne :: [a] -> [a]
alterne [x]=[x]
alterne (x:xs:xss) = x : alterne (xss) 

combine :: (a -> b -> c) -> [a] -> [b] -> [c] 
combine f [] l = []
combine f l [] = []
combine f (x:xs) (y:ys) = f x y : combine f xs ys  

pasPascal :: [Integer] -> [Integer] 
pasPascal (x:xs) = 1 :zipWith (+) xs (x:xs) ++ [1]  

pascal :: [[Integer]] 
pascal = take 10 (iterate  pasPascal [1]) 


type Point = (Float, Float)
type Path  = [Point] 

pointAintercaler :: Point -> Point -> Point 
pointAintercaler (xa,ya) (xb,yb) =((xa+xb)/2+(yb-ya)/2,(ya+yb)/2+(xa-xb)/2)  

pasDragon :: Path -> Path
pasDragon [] =[]
pasDragon [x]= [x]
pasDragon [x,y] = [x,pointAintercaler x y ,y]
pasDragon (x:y:z:xs) = [x]++ [(pointAintercaler x y )] ++[y]++ [pointAintercaler z y]++pasDragon (z:xs) 

dragon :: Point -> Point -> [Path]
dragon (xa,ya) (xb,yb) = iterate pasDragon [(xa,ya),(xb,yb)] 