import Data.List.Zipper
import Optimisation

--type Capacite = [Double]

-- Une Tâche est un triplet (durée, besoins,dateDebut)
data Tache = Tache { duree :: Int,
					besoins :: [Double],
					predecesseurs :: [Tache],
					dateDebut :: Int}
                deriving Show

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


test  p@(Probleme finies cours candidates restantes ressource temps) = 
	let rea = [c |c <- candidates, and (zipWith < (ressources c) (ressource))]
	in rea


--pBranch  p@(Probleme finies cours candidates restantes ressources temps) = 
	--	let f x = let rea = [c |c <- candidates, fold f c ]

		 
{-
pBranch  p@(Probleme finies cours candidates restantes ressources temps) = fst $ until (endp.snd) genererPb ([],z)
    where z = fromList l2
          genererPb (liste,zipper) = let current = cursor zipper
                                         pb = case conf of
                                                   N -> Probleme (current:l1) (toList $ zipper) (cmax - snd current) 
                                                   Boolean -> Probleme (current:l1) (toList $ delete zipper) (cmax - snd current) in
                                     if (snd current <= cmax) then (pb:liste,right zipper)
                                                              else (liste, right zipper)
-}