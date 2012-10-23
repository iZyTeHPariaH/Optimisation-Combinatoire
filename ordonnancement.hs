import Optimisation

type Instant = Double
data Tache = Tache  {duree :: Double,
                     dependances :: [Tache]} deriving Show
type TacheAffectee = (Tache, Instant, Machine)            
             
data Machine = Machine {efficacite :: Double,
                        coutMachine :: Double} deriving Show
                                                        
data Probleme = Probleme {instantActuel :: Instant,
                          tachesAffectees :: [TacheAffectee],
                          tachesRestantes :: [Tache],
                          machinesAffectees :: [Machine],
                          machinesDisponibles :: [Machine]}
                
instance OptNode Probleme where                
  trivial = null . tachesRestantes
  solve = sum . (map (\(t,_,_) -> duree t)) . tachesAffectees

        
-- Affecte une tâche à une machine dans un problème
-- NB que la machine et respectivement la tâche sont supposés ne plus appartenir aux listes
-- de machines disponibles et respectivement de taches restantes et n'y sont donc pas
-- supprimées.

affecterTache :: Tache -> Machine -> Probleme -> Probleme
affecterTache t m p = p {tachesAffectees = (t,instantActuel p,m): tachesAffectees p}
