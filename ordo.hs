--teteete
import Optimisation

--type Capacite = [Double]

-- Une Tâche est un triplet (durée, besoins,dateDebut)
data Tache = Tache { duree :: Double,
					besoins :: [Double],
					dateDebut :: Int}
                deriving Show

-- Un probleme est un triplet (Tâches ordonancées, Tâches candidates, [Capacité restante], temps)
data Probleme = Probleme { ordonancee :: [(Tache)],
                           candidats :: [Tache],
                           capacite :: [Double],
						   temps :: Int}
                deriving Show


-- Définition de notre problème d'optimisation
instance OptNode Probleme where
  {- Le cas trivial se présente quand il n'y a plus de tâches à placer
     ou que nous ne pouvons plus placer d'objets (nous n'avons plus de  
     place) -}
  trivial (Probleme _ c _ _)
    | null c = True
    | otherwise = False
  
  {- Résoudre le problème consiste à sommer les valeurs des objets placés -}
  solve (Probleme _ _ _ t) = t