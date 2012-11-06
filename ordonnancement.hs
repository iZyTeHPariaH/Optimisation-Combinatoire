{- TODO :
-> Améliorer l'accès aux tâches (en remplaçant les listes par des tableaux et en spécifiant les indices
   dans les prédécesseurs.
-> Réduire le degrès des tâches dont on termine les prédécesseurs dans la fonction libererRessources
-> Terminer la fonction tachesCandidates (qui génère l'ensemble des tâches candidates)
-> Dans pBranch, il faut prendre en compte l'efficacité de la machine
-}


import Optimisation
import Data.List
import Data.Array
import Control.Monad.State
import Control.Monad


type Instant = Double
data Tache = Tache {tLabel :: String,
                    dateDebut :: Instant,
                    dureeBase :: Double,
                    predecesseurs :: [Integer],
                    successeurs :: [Integer], -- Liste d'indices pour un accès rapide
                    degre :: Integer}
             
instance Eq Tache where             
  t1 == t2 = (tLabel t1) == (tLabel t2)
  
data Machine = Machine {mLabel :: String,
                        efficacite :: Double}
instance Eq Machine where
  m1 == m2 = mLabel m1 == mLabel m2

type TacheOrdonnancee = (Integer, Machine, Instant)

data Probleme = Probleme {instant :: Instant,
                          taches :: Array Integer Tache, -- Totalité des tâches à réaliser
                          tachesFinies :: [TacheOrdonnancee],
                          tachesEnCours :: [TacheOrdonnancee],
                          tachesRestantes :: [Integer],
                          machinesDisponibles :: [Machine]}
                
type ProblemeS = State Probleme

recupererTache :: Integer -> ProblemeS Tache
recupererTache t = do
  p <- get
  return $ taches p ! t

recupererPredecesseurs :: Tache -> ProblemeS [Tache]
recupererPredecesseurs t = do
  p <- get
  return $ map (taches p !) (predecesseurs t)

recupererSuccesseurs :: Tache -> ProblemeS [Tache]
recupererSuccesseurs t = do
  p <- get
  return $  map (taches p !) (successeurs t)




-- Libère les ressources d'une tâche
terminerTache :: TacheOrdonnancee -> ProblemeS ()
terminerTache e@(i,machine,debut) = do
  p <- get
  tache <- recupererTache i
  -- On récupère les suivants et on réduit leur degrès
  suivants <- recupererSuccesseurs tache
  let nouveauxSuivants = map (\t -> t{degre = degre t - 1}) suivants
      
  -- On modifie le problème en ajoutant l'ensemble aux taches finies,
  -- en le supprimant des tâches en cours, en libérant les ressources
  -- et en diminuant le degrès des successeurs.
  put $ p{tachesFinies = e:tachesFinies p,
          tachesEnCours = tachesEnCours p \\ [e],
          machinesDisponibles = machine:machinesDisponibles p, 
          taches = taches p // zip (successeurs tache) nouveauxSuivants}
  

-- Selectionne les taches candidates à être executées
tachesCandidates :: ProblemeS [Tache]
tachesCandidates = do
  p <- get
  let machinesD = machinesDisponibles p
      indicesTachesR = tachesRestantes p
  tachesRestantes <- foldM (\a e -> recupererTache e >>= \r -> return $ r:a) [] indicesTachesR
  return [t | t <- tachesRestantes, degre t == 0]

                 
pBranch' :: ProblemeS ()
pBranch' = do 
  p <- get
  let t = instant p
      testerDateFin e@(indice,machine,debut) = do
        tache <- recupererTache indice
        if debut + (dureeBase tache)* (efficacite machine) <= t
        then terminerTache e
        else return ()
  -- On libère les ressources des taches en cours qui sont terminées à l'instant actuel
  foldM (\ _ e -> testerDateFin e) () (tachesEnCours p)
  
 
  return ()