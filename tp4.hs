
data Arbre coul val = Noeud coul val (Arbre coul val) (Arbre coul val )| Vide deriving Show  


mapArbre :: (c->c) -> (v->v) -> Arbre c v -> Arbre c v 
mapArbre f g Vide = Vide 
mapArbre f g (Noeud coul val left right) = Noeud (f coul) (g val)  (mapArbre f g left)  (mapArbre f g right) 

hauteur :: Arbre c v -> Int 
hauteur Vide =  0 
hauteur (Noeud c v left right) = 1 + max (hauteur left) (hauteur right) 

taille :: Arbre c v -> Int 
taille Vide = 0 
taille (Noeud c v left right) = 1 +taille left + taille right 


peigneGauche :: [(c,a)] -> Arbre c a 
peigneGauche [] = Vide 
peigneGauche ( x:xs) = Noeud (fst x) (snd x) (peigneGauche xs) Vide  


estParfait :: Arbre c a -> Bool 
estParfait Vide = False
estParfait (Noeud c v left right) = hauteur left == hauteur right && estParfait left && estParfait right  


f :: a -> [a] 
f a = iterate (\x ->x) a 

listAll::[((),Char)]
listAll = [((),a)| a <- ['a'..]] 


aplatit :: Arbre c a -> [(c, a)]
aplatit Vide = []
aplatit (Noeud c v left right) = (c,v) : aplatit (left) ++ aplatit(right) 

element :: Eq a => a -> Arbre c a -> Bool
element a Vide =False 
element a (Noeud c v left right )= if a == v then True else element a (left) || element a (right) 




















