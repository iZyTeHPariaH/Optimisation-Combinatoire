
import Optimisation
import Control.Monad.State
import Control.Monad.Cont
import Debug.Trace
import Data.Array
import Data.List

data Tache = Tache { tLabel :: String,
                     indice :: Integer,
                     duree :: Double,
                     cout :: [Double],
                     predecesseurs :: [Integer],
                     successeurs :: [Integer],
                     degre :: Integer,                    
                     dateDebut :: Double} deriving Show

type TacheOrdonnancee = (Integer, Double)

data Probleme = Probleme {taches :: Array Integer Tache,
                          instant :: Double,
                          tachesFinies :: [TacheOrdonnancee],
                          tachesEnCours :: [TacheOrdonnancee],
                          tachesRestantes :: [Integer],
                          ressources :: [Double]} 
instance Show Probleme where
  show p = "[*] [t=" ++ show (instant p) ++ "] \n tachesFinies = " 
                     ++ show (tachesFinies p) ++ "\n tachesEnCours = "
                     ++ show (tachesEnCours p) ++ "\n tachesRestantes = "
                     ++ show (tachesRestantes p) ++ "\n ressources = "
                     ++ show (ressources p)
instance OptNode Probleme where                
  trivial p = null (tachesRestantes p)
  solve p = if null (tachesEnCours p) then instant p else maximum [dateDebut + duree t | (indice,dateDebut) <- tachesEnCours p, let t = taches p ! indice]
                
type ProblemeS = State Probleme



calcSucc  :: Tache -> ProblemeS ()
calcSucc t = do 
    p <- get
    let indicesPred = predecesseurs t 
        pred = map (taches p !) indicesPred
        ajoutSucc tache = tache{successeurs = indice t : successeurs tache}
    put p{taches = taches p // zip indicesPred (map ajoutSucc pred)}

majSuccesseurs :: ProblemeS ()
majSuccesseurs=do
    tachesT <- gets taches
    foldM (\ _ t -> calcSucc t) () (elems tachesT)



recupererTache :: Integer -> ProblemeS Tache
recupererTache i = do
  t <- gets taches
  return $ t ! i
  
recupererSuccesseurs :: Tache -> ProblemeS [Tache]
recupererSuccesseurs t = do
  p <- get
  return $ map (taches p !) (successeurs t)
  

-- Termine la tâche ordonnancée spécifiée (libère les ressources et réduit le degrès des suivants)
terminerTache :: TacheOrdonnancee -> ProblemeS ()
terminerTache e@(indice,temps)= do
  p <- get
  tache <- recupererTache indice
  suivants <- recupererSuccesseurs tache
 
  put $ p{tachesFinies = e:tachesFinies p,
          tachesEnCours = tachesEnCours p \\ [e],
          taches = taches p // zip (successeurs tache) (map (\t -> t{degre = degre t - 1}) suivants),
          ressources = zipWith (+) (ressources p) (cout tache)}

terminerTaches :: ProblemeS ()
terminerTaches = do
  p <- get
  foldM (\_ e -> terminerTache e) () [(i,debut) | (i,debut) <- tachesEnCours p, 
                                                  let t = taches p ! i,
                                                  debut + duree t <= instant p]
  
-- Retourne l'ensemble des tâches dont les prédécesseurs sont tous terminés 
--(sachant qu'on a assez de ressources  pour les executer)
tachesCandidates :: ProblemeS [Tache]
tachesCandidates = do
  p <- get
  return $ [t | i <- tachesRestantes p, 
                let t = taches p ! i, 
                degre t == 0, 
                and $ zipWith (<=) (cout t) (ressources p)]
  
  
demarerTache :: Tache -> ProblemeS ()
demarerTache t = do
  p <- get
  let tacheOrdo = (indice t, instant p)
  put $ p{taches = taches p // [(indice t, t{dateDebut=instant p})],
          tachesEnCours = tacheOrdo:tachesEnCours p,
          tachesRestantes = tachesRestantes p \\ [indice t],
          ressources = zipWith (-) (ressources p) (cout t)}


-- Attend la fin d'une tâche en cours (incrémente le temps et libère les ressources)
attendreFin :: ProblemeS ()
attendreFin = do
  p <- get
  put $ p{instant = minimum [debut + duree t | (i,debut) <- tachesEnCours p,
                                               let t = taches p ! i]}
  
  
pBranch :: Probleme -> [Probleme]
pBranch p = if null tachesC
            then [snd $ runState attendreFin p']
            else p'{instant = instant p + 1}:map (\t -> snd $ runState (demarerTache t) p') tachesC
    where (tachesC,p') = runState (terminerTaches >> tachesCandidates) p


pert p t tableau = if dateDebut t >= 0
                    then dateDebut t
                   else if degre t == 0
                    then instant p
                   else maximum [pert p ti tableau + duree ti | ti <- map (tableau !) (predecesseurs t)]


-- Utilitaires
trierListe critere liste = sortBy critere' liste
    where a `critere'` b = case a `critere` b of
            True -> LT
            False -> GT
            
f #. 1 = f
f #. n = f . (f #. (n - 1))
choixCandidat p = (if length nxt > 1 then head $ tail nxt
                                    else head $ nxt)
    where nxt = pBranch p
            


pBorne p = max (pBorne1 p) (pBorne2 p)
pEval p = solve $ until trivial choixCandidat p


pBorne1 p = pert p (last $ elems $ taches p) (taches p)

pBorne2 :: Probleme -> Double
pBorne2 p = let tachesC = map ((taches p !).fst) (tachesEnCours p)
                tachesR = map (taches p !) (tachesRestantes p)
                energieTacheRestante t = map (* duree t) (cout t) --Energie pour les taches non commencées
                energieTacheCours t = map (* ((duree t) + (dateDebut t) - (instant p))) (cout t) --Energie pour les taches en cours
                addEnergie l = foldl (zipWith (+)) (repeat 0) l -- Sommer une liste de listes d'energies 
                energie = zipWith (+) (addEnergie $ map energieTacheRestante tachesR) (addEnergie $ map energieTacheCours tachesC) --Energie totale necessaire
                ressourcesDispo = foldl (zipWith (+)) (ressources p) (map cout tachesC)
                ratio = zipWith (/) energie ressourcesDispo
            in (maximum ratio) + (instant p)
 




	   
tachesAFaire' = [Tache  "debut" 0 0 [0,0] [] [],
                Tache  "A" 1 10  [3,0] [0] [],                           
                Tache  "B_prec" 2 2 [0,0] [0] [],
                Tache  "B" 3  4 [3,0] [2] [],
                Tache  "C" 4 2  [1,0] [0] [] ,
                Tache "D" 5 8 [1,1] [1,3] [],
                Tache "E" 6 6 [1,1] [3] [],
                Tache "F" 7 5 [2,1] [4] [],                           
                Tache  "G" 8  9 [3,0] [5,6] [],
                Tache  "H" 9 2 [2,1] [6,7] [],
                Tache  "I" 10 7 [1,0] [8,9] [],
                Tache  "J" 11 4 [2,0] [7] [],
                Tache  "Fin" 12 0 [0,0] [10,11] []]
tachesAFaire = [t {degre = fromIntegral $ length $ predecesseurs t} | t <- map (\f -> f 0 (-1)) tachesAFaire']
p1 = Probleme (array (0,12) (map (\t -> (indice t, t) )tachesAFaire)) 0 [] [] [0..12] [5,1]

startbb p = runCont (branchbound pBranch pBorne pEval p1 (p1, pEval p1) Min (\_ _ -> GT)) print

p2 = Probleme {taches = listArray (0,12) tachesAFaire,
                        instant = 21,
                        tachesFinies = [(7,13.0),(3,10.0),(1,0.0),(4,0.0),(2,0.0),(0,0.0)],
                        tachesEnCours = [(11,21.0),(5,19.0)],
                        tachesRestantes =  [6,8,9,10,12],
                        ressources =  [2.0,0.0]}
						
						
main = startbb p1
