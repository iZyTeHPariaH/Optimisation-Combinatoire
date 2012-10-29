import Data.List.Zipper
import Optimisation
import Data.List
import Control.Monad.Cont


-- Une Tâche est un objet (label, durée, besoins, predecesseurs, degre, dateDebut)
data Tache = Tache {label :: String,
                    duree :: Integer,
                    besoins :: [Double],
                    predecesseurs :: [String],
					degre :: Integer,
                    dateDebut :: Integer}
                deriving Show
-- Les tâches sont identifiées par leur label
-- elles sont donc égales lorsque leurs label sont égaux                
instance Eq Tache where
  t1 == t2 = label t1 == label t2
  
  
  
  
-- toString pour des liste d'objets, on affiche chaque attribut entre corchet sur une ligne 
showList' l = if null l then "[]"						
              else "["++ foldl1 (\a e -> a  ++ "\n" ++ e) (map show l) ++"]"
  
  
  
  
-- Un probleme est un objet : 
--   (Tâches finies, Tâches en cours, Tâches candidates, Tâches restantes, [ressources restante], temps)
data Probleme = Probleme { finies :: [(Tache)],
                           cours :: [(Tache)],
                           candidates :: [(Tache)],
                           restantes :: [Tache],
                           ressources :: [Double],
                           temps :: Integer}
							
instance Show Probleme where
 show p = "\n\nTaches Finies :\n" ++ showList' (finies p) ++"\n" ++ 
          "Taches en cours :\n" ++ showList' (cours p) ++"\n" ++ 
          "Taches candidates :\n" ++showList' (candidates p) ++"\n" ++ 
          "Taches restantes :\n" ++ showList' (restantes p) ++ "\n" ++ 
          "Ressources :\n" ++ show (ressources p) ++ "\n"++
          "Temps :" ++show (temps p)

--instance Ord Probleme where compare = heuristiqueBB
			
			

-- Définition de notre problème d'optimisation
instance OptNode Probleme where
  {- Le cas trivial se présente quand il n'y a plus de tâches en candidates ni restantes à placer -}
  trivial (Probleme _ _ c r _ _)
    | (null r) && (null c) = True
    | otherwise = False
  
  {- Résoudre le problème consiste à retourner le temps total de l'ordonancement
		i.e. le maximum des (dates débuts + durées) des tâches en cours-}
  solve (Probleme _ cours _ _ _ _) = fromIntegral $ maximum [dateDebut c + duree c |c <- cours]
  
  
  
  
  
  
  
  
  {- La séparation consiste à :
		Si parmis les taches candidates j'en ai qui sont réalisables (suffisement de ressources)
			je retourne tous les pb avec une de ces taches en plus, en calculant les ressources restantes
		Sinon
			sortir la prochaine tâche qui se termine dans {taches en cours}
			ajouter le temps
			liberer les ressources
			recalculer taches candidates
-}
pBranch p = let candidatsSortants = [(t,dateDebut t + duree t) | t <- cours p]
--				Tache en tete de la liste candidats Sortants
                premierCandidatSortant = head $ candidatsSortants
--				Taches realisables : candidates et suffisement de ressources sont dispo
                rea = [c | c <- candidates p, and $ zipWith (<=) (besoins c) (ressources p)]
--				Liste des prochaines taches à finir
                meilleursCandidatsSortants = foldl (\(a,d) (t,dFin) -> if null a then ([t],dFin)
                                                                       else if d == dFin then (t:a,d)
                                                                       else if dFin < d then ([t],dFin)
                                                                       else (a,d)) ([],temps p) candidatsSortants
--																	   
                probleme1 = p{cours = cours p \\ fst meilleursCandidatsSortants,
                              temps = snd meilleursCandidatsSortants,
							  --On supprime les predecesseurs de restantes parmis celle qu'on vient de sortir 
                              restantes =  map (\t -> t{predecesseurs = predecesseurs t \\ map label (fst meilleursCandidatsSortants) }) (restantes p),
							  --On ajoute aux taches finies celles qui sortent
                              finies = finies p ++ fst meilleursCandidatsSortants,
                              ressources = foldl (zipWith (+)) (ressources p) (map besoins (fst meilleursCandidatsSortants)) }
--				Taches n'ayant plus de successeurs			  
                nouveauxCandidats = [c | c <- restantes probleme1, null (predecesseurs c)]                                
--		   S'il n'y a pas de taches realisables
		   in if null rea
		       then [probleme1{restantes = restantes probleme1 \\ nouveauxCandidats,
                                       candidates = candidates probleme1 ++ nouveauxCandidats}]
                       else p{temps = temps p + 1}:map f rea
         where f tache = p{cours = tache{dateDebut=temps p}: cours p,
                           candidates = (candidates p) \\ [tache], --tail $ dropWhile (/= tache) (candidates p),
                           ressources = zipWith (-) (ressources p) (besoins tache)}
		        

 
-- date de fin
pert t l = case pred of
    [] -> if dateDebut t < 0 then 0 else duree t --si elles n'ont pas de predecesseurs et ont un temps négatif, elles sont candidates, les autres sont en cours
    otherwise -> maximum [pert ti l + duree ti | ti <- pred]
 where pred = [t' | t' <- l, label t' `elem` predecesseurs t]

 

{- Heuristique servant à trier les neuds lors de l'utilisation de A* 
	dans le calcul de l'évaluation -}
heuristique p1 p2 = calc p1 <= calc p2
		where calc p = (temps p + sum (map duree (restantes p ++ candidates p)))

		
{- La borne non optimale réalisable consiste à appliquer A*
	c'est a dire prendre le meilleur noeud parmis les fils, suivant une heuristique -}
pEval p = snd $ astar pBranch heuristique p


{- La borne optimale non réalisable est la solution du problème relaxé. 
		On abandonne les contraintes de ressources -}
		
pBorne p = fromInteger (minTachesEnCours + pert (last reste) reste)
  where reste = concat [candidates p, restantes p]
        minTachesEnCours = case cours p of
          [] -> temps p                
          otherwise -> minimum [dateDebut t' + duree t' | t' <-cours p]

		
		
		
heuristiqueBB p1 p2 = GT


tachesAFaire2 = [Tache "A0" 0 [0,0] [],
				Tache "A1" 6  [2,1] ["A0"],
				Tache "A2" 1 [1,0] ["A0"],
				Tache "A3" 1 [3,1] ["A0"],
				Tache "A4" 2 [2,0] ["A0"],
				Tache "A5" 3 [1,1] ["A2"],
				Tache "A6" 5 [2,1] ["A2"],
				Tache "A7" 6 [3,0] ["A3"],
				Tache "A8" 3 [1,2] ["A4"],
				Tache "A9" 2 [1,2] ["A5"],
				Tache "A10" 4 [1,1] ["A1","A6","A9"],
				Tache "A11" 0 [0,0] ["A0","A10","A8","A9","A7"]]
		
		
		
		
tachesAFaire = [Tache  "debut"  0 [0,0] [] ,
                Tache  "A" 10  [3,0] ["debut"] ,                           
                Tache  "B_prec"  2 [0,0] ["debut"],
                Tache  "B"   4 [3,0] ["B_prec"] ,
                Tache  "C"  2  [1,0] ["debut"] ,
                Tache "D" 8 [1,1] ["A","B"],
                Tache "E" 6 [1,1] ["B"],
                Tache "F" 5 [2,1] ["C"],                           
                Tache  "G"  9 [3,0] ["D","E"],
                Tache  "H"  2 [2,1] ["E","F"],
                Tache  "I"  7 [1,0] ["G","H"],
                Tache  "J"  4 [2,0] ["F"],
                Tache  "Fin"  0 [0,0] ["I","J"]]

				
taches = [t{degre = fromIntegral $ length $ predecesseurs t} | tgen <- tachesAFaire, let t = tgen 0 (-1) ]
p = Probleme [] [] [head taches] (tail taches) [5,1] 0

taches2 = [t{degre = fromIntegral $ length $ predecesseurs t} | tgen <- tachesAFaire2, let t = tgen 0 (-1) ]
p2 = Probleme [] [] [head taches2] (tail taches2) [7,4] 0

f1 #. 1 = f1
f1 #. n = f1.(f1 #. (n-1))

choixCandidat p = foldl1 (\a e -> if a `heuristique` e then a else e) (pBranch p)



startBranchBound p = runCont (branchbound pBranch pBorne pEval p (p, pEval p) Min heuristiqueBB) print


file = "D:\\My Documents\\Cours\\Git\\Optimisation-Combinatoire\\Log.txt"

{-
main = do
        writeFile file (runCont (branchbound pBranch pBorne pEval p (p, pEval p) Min) show)
-}



































