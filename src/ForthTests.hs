module ForthTests
    ( doRealRecursion
    ) where

import Polysemy (Members, Sem, reinterpret, run)
import Polysemy.Input (Input(Input), input, runInputConst)
import Polysemy.Output (Output, output, runLazyOutputList)
import Polysemy.State (get, put, runState)

import Pair (Pair(Pair))


testProgram :: Members '[Input Int, Input String, Output String] r => Sem r String
testProgram = do n :: Int <- input
                 output $ (show n) ++ "?"
                 i1 :: String <- input
                 output $ "Success" ++ i1
                 i2 <- input
                 return $ (init i2) ++ "!"

doRealRecursion :: IO ()
doRealRecursion = do let programs = mkTest <$> (Pair 1 2)
                     print $ "Attempting " ++ (show $ length programs) ++ " testPrograms."
                     let Pair (os1, m1) (os2, m2) = run <$> runParallel id programs
                     print $ "Outputs: " ++ (show $ Pair os1 os2)
                     print $ "Messages: " ++ (show $ Pair m1 m2)
  where mkTest :: Members '[Input String, Output String] r => Int -> Sem r String
        mkTest i = runInputConst i testProgram

runParallel :: forall x a r.
               (Sem r ([x], a) -> Sem '[] ([x], a)) ->
               Pair (Sem (Input x ': Output x ': r) a) ->
               Pair (Sem r ([x], a))
runParallel handleRest sems = runLazyOutputList <$> (runComInList <$> (Pair os2 os1) <*> sems)
  where Pair os1 os2 :: Pair [x] = fst <$> run <$> handleRest <$> runParallel handleRest sems
        runComInList inpts = (snd <$>) . (runState inpts) . (reinterpret (\case
            Input -> do ~(s : ss) <- get @[x]
                        put @[x] ss
                        pure s
            ))



