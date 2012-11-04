import Data.List.Zipper
import Control.Monad.Cont
import Optimisation



-- Sac a dos
type Poid = Double
type Capacite = Double

type Objet = (Double, Poid)

-- Un probleme est un triplet (Objets placés, Objets candidats, Capacité restante)
data Probleme = Probleme { contenu :: [(Objet)],
                           candidats :: [Objet],
                           capacite :: Capacite}
                deriving Show

data ProblemConf = N | Boolean

-- Définition de notre problème d'optimisation
instance OptNode Probleme where
  {- Le cas trivial se présente quand il n'y a plus d'objets à placer
     ou que nous ne pouvons plus placer d'objets (nous n'avons plus de  
     place) -}
  trivial (Probleme _ cd cmax)
    | null [c | c <- cd , snd c <= cmax] = True
    | otherwise                          = False
  
  {- Résoudre le problème consiste à sommer les valeurs des objets placés -}
  solve (Probleme l _ _) = sum (map fst l)
  


{- La séparation consiste à ajouter un élément au sac a dos, en respectant la contrainte
   de capacité -}
pBranch conf p@(Probleme l1 l2 cmax) = fst $ until (endp.snd) genererPb ([],z)
    where z = fromList l2
          genererPb (liste,zipper) = let current = cursor zipper
                                         pb = case conf of
                                                   N -> Probleme (current:l1) (toList $ zipper) (cmax - snd current) 
                                                   Boolean -> Probleme (current:l1) (toList $ delete zipper) (cmax - snd current) in
                                     if (snd current <= cmax) then (pb:liste,right zipper)
                                                              else (liste, right zipper)
                                     
                                    
{- Le meilleur candidat à placer dans le sac est celui qui est le plus rentable (donc qui maximise le ratio 
   valeur / poids). -}
meilleurCandidat (Probleme l1 l2 cmax) = foldl1 (\a@(i,r,c) e@(i',r',c') -> if r > r' then a else e ) candidats
    where candidats = zip3 [0..] (map (\(val,poid) -> val / poid) l2 ) l2 

{- La borne optimale non réalisable est la solution du problème relaxé. On abandonne les contraintes de valeur 
   entières,et on prend le plus possible du meilleur candidat. -}
pBorne p@(Probleme l1 l2 cmax) = solve p + cmax * maximum [vi / bi | (vi, bi) <- l2]

{- La borne non optimale réalisable consiste à tronquer la quantité prise du meilleur candidat, pour qu'elle 
   respecte les contraintes de valeurs entière -}
pEval p = solve p + (fromInteger $ truncate $ capacite p /(snd c))*(fst c)
    where (i,r,c) = meilleurCandidat p


{- Exemple :
   _____________________
   |Nom   | x1 | x2 | x3|
   +______+____+____+___+
   |Valeur| 10 | 8  | 5 | 
   +______+____+____+___+___+
   |Poid  | 6  | 5  | 4 | 9 |
   +______+____+____+___+___+  -}
p1 = Probleme [] [(10,6), (8,5), (5,4)] 9
p2 = Probleme [] [(7,13), (4,12), (3,8), (3,10)] 30

{- Résolution de l'exemple. -}
test = branchbound (pBranch Boolean) pBorne pEval p1 (p1, pEval p1) Max

startBranchBound p = runCont (branchbound (pBranch N) pBorne pEval p (p, 0) Max) print

