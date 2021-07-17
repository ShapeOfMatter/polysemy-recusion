module FifthTests
    ( doRealRecursion,
      transposeVlV
    ) where

import Control.Applicative (ZipList(ZipList), getZipList)
import Data.Distributive (distribute)
import Data.Fin (Fin)
import Data.List (isInfixOf)
import Data.Nat (Nat(S, Z))
import Data.Vec.Lazy (Vec, universe, (!))
import Data.Type.Nat (SNatI)
import Polysemy (Members, Sem, reinterpret, run)
import Polysemy.Input (Input(Input), input, runInputConst)
import Polysemy.Output (Output, output, runLazyOutputList)
import Polysemy.State (get, put, runState)

type Three = 'S ('S ('S 'Z))

testProgram :: forall (n :: Nat) r.
               SNatI n =>
               Members '[Input (Fin n), Input (Vec n String), Output (Vec n String)] r => Sem r String
testProgram = do b :: Fin n <- input
                 let name = "P" ++ (show b)
                 output $ mkMessages $ const $ "Hi I'm " ++ name ++ "!"
                 i1 <- input
                 let others = reverse . (take 2) . reverse . init <$> i1
                 output $ mkMessages $ ("Ok, you're " ++) . (++ ".") . (others !)
                 i2 :: Vec n String <- input
                 let success = all (isInfixOf name) i2
                 return $ if success then "Success!" else "Failure!"
  where mkMessages f = mkMessage f <$> universe
        mkMessage f p = "For " ++ (show p) ++ ": " ++ (f p)

doRealRecursion :: IO ()
doRealRecursion = do let programs = mkTest <$> universe @(Three)
                     print $ "Attempting " ++ (show $ length programs) ++ " testPrograms."
                     let results = run <$> runParallel id programs
                     print $ "Outputs: " ++ (show $ fst <$> results)
                     print $ "Inputs: " ++ (show $ transposeVlV $ fst <$> results) 
                     print $ "Messages: " ++ (show $ snd <$> results)
  where mkTest :: SNatI n => Members '[Input (Vec n String), Output (Vec n String)] r => Fin n -> Sem r String
        mkTest b = runInputConst b testProgram

runParallel :: forall x a r (n :: Nat).
               SNatI n =>
               (Sem r ([Vec n x], a) -> Sem '[] ([Vec n x], a)) ->
               Vec n (Sem (Input (Vec n x) ': Output (Vec n x) ': r) a) ->
               Vec n (Sem r ([Vec n x], a))
runParallel handleRest sems = runLazyOutputList <$> (runComInList <$> (transposeVlV os) <*> sems)
  where os :: Vec n [Vec n x] = fst <$> run <$> handleRest <$> runParallel handleRest sems
        runComInList inpts = (snd <$>) . (runState inpts) . (reinterpret (\case
            Input -> do ~(s : ss) <- get @[Vec n x]
                        put @[Vec n x] ss
                        pure s
            ))

transposeVlV :: forall (n :: Nat) a.
                SNatI n =>
                Vec n [Vec n a] -> Vec n [Vec n a]
transposeVlV = transposeLV . (transposeVV <$>) . transposeVL
  where transposeLV :: forall a'. [Vec n a'] -> Vec n [a']
        transposeLV = distribute
        transposeVV :: forall (n' :: Nat) (n'' :: Nat) a'.
                       (SNatI n', SNatI n'') =>
                       Vec n' (Vec n'' a') -> Vec n'' (Vec n' a')
        transposeVV = traverse id
        transposeVL :: forall a'. Vec n [a'] -> [Vec n a']
        transposeVL = getZipList . traverse ZipList

