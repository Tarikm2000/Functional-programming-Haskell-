
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Graphics.Gloss 
import Graphics.Gloss.Data.Picture
import Data.Text.Unsafe (iter)
type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot] 


motSuivant :: Regles -> Mot -> Mot 
motSuivant r [] = [] 
motSuivant r (x:xs) = r x ++ motSuivant r xs  

motSuivant' :: Regles -> Mot -> Mot 
motSuivant' r l = concatMap r l  


motSuivantWithComp :: Regles -> Mot -> Mot
motSuivantWithComp f xs =concat [ f x | x<-xs ]

flocon :: Symbole ->Mot
flocon '-'="-"
flocon '+'="+"
flocon 'f' = ['f','-','f','+','+','f','-','f'] 

lsysteme :: Axiome -> Regles -> LSysteme 
lsysteme a r = iterate (motSuivant r) a   

type EtatTortue = (Point, Float)   
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue  


etatInitial :: Config -> EtatTortue
etatInitial (ei,li,fe,ang,sy) = ei 

longueurPas :: Config -> Float
longueurPas (ei,li,fe,ang,sy)=li

facteurEchelle :: Config -> Float
facteurEchelle  (ei,li,fe,ang,sy) = fe 

angle :: Config -> Float
angle (ei,li,fe,ang,sy) = ang

symbolesTortue :: Config -> [Symbole]
symbolesTortue (ei,li,fe,ang,sy) = sy 


avance :: Config -> EtatTortue -> EtatTortue
avance c (p,cap) = ((fst(p)+longueurPas c *cos (cap),snd(p)+longueurPas c *sin (cap)),cap)  

tourneAgauche::Config -> EtatTortue -> EtatTortue
tourneAgauche conf (p,cap) = (p,cap + (angle conf)) 

tourneAdroite::Config -> EtatTortue -> EtatTortue
tourneAdroite conf (p,cap) = (p,cap - (angle conf)) 

filtreSymbolesTortue :: Config -> Mot -> Mot 
filtreSymbolesTortue (a,b,c,d,e) [] = []
filtreSymbolesTortue (a,b,c,d,e) (x:xs) = if x `elem` e then x : filtreSymbolesTortue (a,b,c,d,e) (xs) else filtreSymbolesTortue (a,b,c,d,e) xs  