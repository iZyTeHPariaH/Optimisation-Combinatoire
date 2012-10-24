import Data.List.Zipper
import Optimisation
import Data.List

--type Capacite = [Double]

-- Une T�che est un triplet (dur�e, besoins,dateDebut)
data Tache = Tache {label :: String,
                    duree :: Int,
                    besoins :: [Double],
                    predecesseurs :: [String],
                    dateDebut :: Int}
                deriving Show
                
instance Eq Tache where
  t1 == t2 = label t1 == label t2
  
  
showList' l = if null l then "[]"
						else "["++ foldl1 (\a e -> a  ++ "\n" ++ e) (map show l) ++"]"
  
  
-- Un probleme est un triplet (T�ches finies, T�ches en cours, T�ches candidates, T�ches restantes, [ressources restante], temps)
data Probleme = Probleme { finies :: [(Tache)],
							cours :: [(Tache)],
							candidates :: [(Tache)],
							restantes :: [Tache],
							ressources :: [Double],
							temps :: Int}
							
instance Show Probleme where
 show p = "\n\nTaches Finies :\n" ++ showList' (finies p) ++"\n" ++ 
			"Taches en cours :\n" ++ showList' (cours p) ++"\n" ++ 
			"Taches candidates :\n" ++showList' (candidates p) ++"\n" ++ 
			"Taches Taches restantes :\n" ++ showList' (restantes p) ++ "\n" ++ 
			"Ressources :\n" ++ show (ressources p) ++ "\n"++
			"Temps :" ++show (temps p)


-- D�finition de notre probl�me d'optimisation
instance OptNode Probleme where
  {- Le cas trivial se pr�sente quand il n'y a plus de t�ches � restantes � placer -}
  trivial (Probleme _ _ _ r _ _)
    | null r = True
    | otherwise = False
  
  {- R�soudre le probl�me consiste � retourner le temps total de l'ordonancement
		i.e. le maximum des (dates d�buts + dur�es) des t�ches en cours-}
  solve (Probleme _ cours _ _ _ _) = fromIntegral $ maximum [dateDebut c + duree c |c <- cours]
  
  
  {- La s�paration consiste � :
		Si parmis les taches candidates j'en ai qui sont r�alisables (suffisement de ressources)
			je retourne tous les pb avec une de ces taches en plus, en calculant les ressources restantes
		Sinon
			sortir la prochaine t�che qui se termine dans {taches en cours}
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
         where f tache = p{cours = tache{dateDebut=temps p}: cours p,
                           candidates = tail $ dropWhile (/= tache) (candidates p),
                           ressources = zipWith (-) (ressources p) (besoins tache)
                           }
		        

 
-- date de fin
pert t l = case pred of
    [] -> 0
    otherwise -> maximum [pert ti l + duree ti | ti <- pred]
 where pred = [t' | t' <- l, label t' `elem` predecesseurs t]
 

heuristique p1 p2 = calc p1 <= calc p2
		where calc p = (temps p + sum (map duree (restantes p ++ candidates p)))


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

				
taches = map ($(-1)) tachesAFaire

p = Probleme [] [] [head taches] (tail taches) [5,1] 0


taches2 = map ($(-1)) tachesAFaire2
p2 = Probleme [] [] [head taches2] (tail taches2) [5,1] 0


