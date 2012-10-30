import Optimisation
import Data.List
import Control.Monad.State
type Instant = Double
data Tache = Tache {tLabel :: String,
                    dateDebut :: Instant,
                    dureeBase :: Double,
                    predecesseurs :: [Tache],
                    degres :: Integer}
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

libererRessources t@(tache,machine,debut) = do
  p <- get
  put $ p{tachesFinies = t:tachesFinies p,
          tachesEnCours = tachesEnCours p \\ [t],
          machinesDisponibles = machine:machinesDisponibles p }
  
                 
pBranch' :: ProblemeS [ProblemeS]
pBranch' = do 
  p <- get
  let t = instant p
      -- On considère l'ensemble des tâches se terminant à l'instant actuel
      -- Il faudra remplacer dureeBase par un calcul prenant en compte
      -- l'efficacité de la machine
      taches = [tache | (tache,machine,debut) <- tachesEnCours p,
                        debut + dureeBase tache <= t]
   foldl (\m a -> )    