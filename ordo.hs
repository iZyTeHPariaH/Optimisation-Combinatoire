import Data.List.Zipper
import Optimisation
import Data.List

--type Capacite = [Double]

-- Une Tâche est un triplet (durée, besoins,dateDebut)
data Tache = Tache {label :: String,
                    duree :: Int,
                    besoins :: [Double],
                    predecesseurs :: [String],
                    dateDebut :: Int}
                deriving Show
                
instance Eq Tache where
  t1 == t2 = label t1 == label t2
  
  
-- Un probleme est un triplet (Tâches finies, Tâches en cours, Tâches candidates, Tâches restantes, [ressources restante], temps)
data Probleme = Probleme { finies :: [(Tache)],
							cours :: [(Tache)],
							candidates :: [(Tache)],
							restantes :: [Tache],
							ressources :: [Double],
							temps :: Int}
                deriving Show


-- Définition de notre problème d'optimisation
instance OptNode Probleme where
  {- Le cas trivial se présente quand il n'y a plus de tâches à restantes à placer -}
  trivial (Probleme _ _ _ r _ _)
    | null r = True
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
                rea = [c | c <- candidates p, and $ zipWith (<) (besoins c) (ressources p)]
                meilleursCandidatsSortants = foldl (\(a,d) (t,dFin) -> if null a then ([t],dFin)
																					 else if d == dFin then (t:a,d)
																					 else if dFin < d then ([t],dFin)
																					 else (a,d)) ([],0) candidatsSortants
                probleme1 = p{cours = cours p \\ fst meilleursCandidatsSortants,
								temps = snd meilleursCandidatsSortants,
								restantes =  map (\t -> t{predecesseurs = predecesseurs t \\ map label (fst meilleursCandidatsSortants) }) (restantes p),
								finies = finies p ++ fst meilleursCandidatsSortants,
								ressources = foldl (zipWith (+)) (ressources p) (map besoins (fst meilleursCandidatsSortants)) }
                nouveauxCandidats = [c | c <- restantes probleme1, null (predecesseurs c)]                                
		    in if null rea
		       then [probleme1{restantes = restantes probleme1 \\ nouveauxCandidats,
                        candidates = candidates probleme1 ++ nouveauxCandidats}]
			   else p{temps = temps p + 1}:map f rea
         where f tache = p{cours = tache: cours p,
                           candidates = tail $ dropWhile (/= tache) (candidates p),
                           ressources = zipWith (-) (ressources p) (besoins tache)
                           }
		        

 
-- date de fin
pert t l = case pred of
    [] -> duree t
    otherwise -> duree t + maximum [pert ti l + duree ti | ti <- pred]
 where pred = [t' | t' <- l, label t' `elem` predecesseurs t]
 

heuristique p1 p2 = calc p1 <= calc p2
		where calc p = (temps p + sum (map duree (restantes p ++ candidates p)))


