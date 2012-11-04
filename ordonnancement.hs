{- TODO :
-> Améliorer l'accès aux tâches (en remplaçant les listes par des tableaux et en spécifiant les indices
   dans les prédécesseurs.
-> Réduire le degrès des tâches dont on termine les prédécesseurs dans la fonction libererRessources
-> Terminer la fonction tachesCandidates (qui génère l'ensemble des tâches candidates)
-> Dans pBranch, il faut prendre en compte l'efficacité de la machine
-}


import Optimisation
import Data.List
import Control.Monad.State
import Control.Monad
type Instant = Double
data Tache = Tache {tLabel :: String,
                    dateDebut :: Instant,
                    dureeBase :: Double,
                    predecesseurs :: [Tache],
                    degre :: Integer}
instance Eq Tache where             
  t1 == t2 = (tLabel t1) == (tLabel t2)
  
data Machine = Machine {mLabel :: String,
                        efficacite :: Double}
instance Eq Machine where
  m1 == m2 = mLabel m1 == mLabel m2

type TacheOrdonnancee = (Tache, Machine, Instant)

data Probleme = Probleme {instant :: Instant,
                          tachesFinies :: [TacheOrdonnancee],
                          tachesEnCours :: [TacheOrdonnancee],
                          tachesRestantes :: [Tache],
                          machinesDisponibles :: [Machine]}
                
type ProblemeS = State Probleme

-- Libère les ressources d'une tâche
libererRessources :: TacheOrdonnancee -> ProblemeS ()
libererRessources t@(tache,machine,debut) = do
  p <- get
  put $ p{tachesFinies = t:tachesFinies p,
          tachesEnCours = tachesEnCours p \\ [t],
          machinesDisponibles = machine:machinesDisponibles p }
  
-- Selectionne les taches candidates à être executées
tachesCandidates :: ProblemeS [Tache]
tachesCandidates = do
  p <- get
  let machinesD = machinesDisponibles p
      tachesR = tachesRestantes p
      tachesCandidates = [tache | tache <- tachesR, degre tache == 0]
  return tachesCandidates
                 
pBranch' :: ProblemeS ()
pBranch' = do 
  p <- get
  let t = instant p
      -- On considère l'ensemble des tâches se terminant à l'instant actuel
      -- Il faudra remplacer dureeBase par un calcul prenant en compte
      -- l'efficacité de la machine
      taches = [tacheOrdo | tacheOrdo@(tache,machine,debut) <- tachesEnCours p,
                        debut + dureeBase tache <= t]
  -- On libère les ressources des taches en cours qui sont terminées à l'instant actuel
  foldM (\_ t -> libererRessources t) () taches   
  
  