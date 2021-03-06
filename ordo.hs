import Data.List.Zipper
import Optimisation
import Data.List
import Control.Monad.Cont


-- Une T�che est un objet (label, dur�e, besoins, predecesseurs, degre, dateDebut)
data Tache = Tache {label :: String,
                    duree :: Integer,
                    besoins :: [Double],
                    predecesseurs :: [String],
					degre :: Integer,
                    dateDebut :: Integer}
                deriving Show
-- Les t�ches sont identifi�es par leur label
-- elles sont donc �gales lorsque leurs label sont �gaux                
instance Eq Tache where
  t1 == t2 = label t1 == label t2
  
  
  
  
-- toString pour des liste d'objets, on affiche chaque attribut entre corchet sur une ligne 
showList' l = if null l then "[]"						
              else "["++ foldl1 (\a e -> a  ++ "\n" ++ e) (map show l) ++"]"
  
  
  
  
-- Un probleme est un objet : 
--   (T�ches finies, T�ches en cours, T�ches candidates, T�ches restantes, [ressources restante], temps)
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
			
			

-- D�finition de notre probl�me d'optimisation
instance OptNode Probleme where
  {- Le cas trivial se pr�sente quand il n'y a plus de t�ches en candidates ni restantes � placer -}
  trivial (Probleme _ _ c r _ _)
    | (null r) && (null c) = True
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
--				Tache en tete de la liste candidats Sortants
                premierCandidatSortant = head $ candidatsSortants
--				Taches realisables : candidates et suffisement de ressources sont dispo
                rea = [c | c <- candidates p, and $ zipWith (<=) (besoins c) (ressources p)]
--				Liste des prochaines taches � finir
                meilleursCandidatsSortants = foldl (\(a,d) (t,dFin) -> if null a then ([t],dFin)
                                                                       else if d == dFin then (t:a,d)
                                                                       else if dFin < d then ([t],dFin)
                                                                       else (a,d)) ([],temps p) candidatsSortants
--				fonction qui decremente de x le degre de la tache t (\t x -> t{degre = (degre t) - x})
                f (t, x) = t{degre = (degre t) - x}
--				fonction qui retourne le couple (t, nombre doccurrences en commun entre l et les predecesseurs de t  (\t l -> (t, length $ intersect (map label (fst meilleurCandidatsSortants)) (predecesseurs t))--
                g l t = (t, fromIntegral $ length $ intersect l (predecesseurs t))--
--																	   
                probleme1 = p{cours = cours p \\ fst meilleursCandidatsSortants,
                              temps = snd meilleursCandidatsSortants,
                              --On supprime les predecesseurs de restantes parmis celle qu'on vient de sortir 
                              --restantes =  map (\t -> t{predecesseurs = predecesseurs t \\ map label (fst meilleursCandidatsSortants) }) (restantes p),
                              restantes = map f $ map (g (map label (fst meilleursCandidatsSortants))) (restantes p),
                              --On ajoute aux taches finies celles qui sortent
                              finies = finies p ++ fst meilleursCandidatsSortants,
                              --On libere les ressources des taches sortantes
                              ressources = foldl (zipWith (+)) (ressources p) (map besoins (fst meilleursCandidatsSortants)) }
                            --Taches n'ayant plus de successeurs			  
                nouveauxCandidats = [c | c <- restantes probleme1, (degre c)==0]                                
--		   S'il n'y a pas de taches realisables
		   in if null rea
		      then [probleme1{restantes = restantes probleme1 \\ nouveauxCandidats,
                               candidates = candidates probleme1 ++ nouveauxCandidats}]
              else (map f' rea) ++ [p{temps = temps p + 1,
					 finies = finies p ++ candidatsSortantsBis,
                     cours = cours p \\ candidatsSortantsBis,
                     restantes = map f $ map (g (map label candidatsSortantsBis)) (restantes p),
                     ressources = foldl (zipWith (+)) (ressources p) (map besoins candidatsSortantsBis) }]
              where f' tache = p{cours = tache{dateDebut=temps p}: cours p,
                                 candidates = (candidates p) \\ [tache], --tail $ dropWhile (/= tache) (candidates p),
                                 ressources = zipWith (-) (ressources p) (besoins tache)}
                    candidatsSortantsBis = [t | t <- cours p, dateDebut t + duree t <= (temps p) + 1]--faut il mettre le +1 ? cf ligne else...

 
-- date de fin
pert t l instant 
	| dateDebut t >= 0 = dateDebut t
	| dateDebut t < 0 && degre t == 0 = instant
	| otherwise = maximum [pert ti l instant + duree ti | ti <- pred]
                  where pred = [t' | t' <- l, label t' `elem` predecesseurs t]
 

{- Heuristique servant � trier les neuds lors de l'utilisation de A* 
	dans le calcul de l'�valuation -}
heuristique p1 p2 = calc p1 <= calc p2
		where calc p = (temps p + sum (map duree (restantes p ++ candidates p)))

		
{- La borne non optimale r�alisable consiste � appliquer A*
	c'est a dire prendre le meilleur noeud parmis les fils, suivant une heuristique -}
pEval p = snd $ astar pBranch heuristique p


{- La borne optimale non r�alisable est la solution du probl�me relax�. 
		On abandonne les contraintes de ressources -}
		
pBorne1 p = fromInteger (pert (last reste) reste (temps p))
  where reste = concat [finies p, cours p, candidates p, restantes p]
        minTachesEnCours = case cours p of
          [] -> temps p                
          otherwise -> minimum [dateDebut t' + duree t' | t' <-cours p]

		  
		  
pBorne2 p = let f t = map (* fromIntegral (duree t)) (besoins t)  --Energie pour les taches non commenc�
                f' t = map (* fromIntegral ((duree t)+(dateDebut t)-(temps p))) (besoins t) --Energie pour les taches en cours
                g l = foldl (zipWith (+)) [0 | t <- ressourcesDispo] l -- Sommer une liste de listes d'energies 
                energie = zipWith (+) (g $ map f ((restantes p)++(candidates p))) (g $ map f' (cours p)) --Energie totale necessaire
                ressourcesDispo = foldl (zipWith (+)) (ressources p) (map besoins (cours p))
                ratio = zipWith (/) energie ressourcesDispo
			 in (maximum ratio) + fromIntegral (temps p)
			 
pBorne p = max (pBorne1 p) (pBorne2 p)		
		
		
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



































