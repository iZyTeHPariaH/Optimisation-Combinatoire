{- Primitives pour l'automatisation de la génération de programmes Cplex -}

import Control.Monad.Writer

type Cplex = Writer String

type VarLabel = String

data CplexCriteria = Maximize | Minimize
data CplexConstraintOperator = GreaterThan | GreaterOrEqual | Equal | LowerOrEqual | Lower

instance Show CplexConstraintOperator where
  show o = case o of
    GreaterThan -> ">"
    GreaterOrEqual -> ">="
    Equal -> "="
    LowerOrEqual -> "<="
    Lower -> "<"

telln s = tell (s ++ "\n")


setCriteria c =  case c of 
  Maximize -> telln "Maximize"
  Minimize -> telln "Minimize"


setObj var = telln ("obj : " ++ var)


addConstraint coeffList operator val = telln $ (foldl (\a e -> a ++ " + " ++ e) [] (map (++ " ") coeffList)) ++ show operator ++ val 