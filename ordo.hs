--teteete
import Optimisation

--type Capacite = [Double]

-- Une T�che est un triplet (dur�e, besoins,dateDebut)
data Tache = Tache { duree :: Double,
					besoins :: [Double],
					dateDebut :: Int}
                deriving Show

-- Un probleme est un triplet (T�ches ordonanc�es, T�ches candidates, [Capacit� restante], temps)
data Probleme = Probleme { ordonancee :: [(Tache)],
                           candidats :: [Tache],
                           capacite :: [Double],
						   temps :: Int}
                deriving Show


-- D�finition de notre probl�me d'optimisation
instance OptNode Probleme where
  {- Le cas trivial se pr�sente quand il n'y a plus de t�ches � placer
     ou que nous ne pouvons plus placer d'objets (nous n'avons plus de  
     place) -}
  trivial (Probleme _ c _ _)
    | null c = True
    | otherwise = False
  
  {- R�soudre le probl�me consiste � sommer les valeurs des objets plac�s -}
  solve (Probleme _ _ _ t) = t