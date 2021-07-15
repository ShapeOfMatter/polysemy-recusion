module SndTests (
  doParallelRecursion,
  parallelEvaluation,
  runUnsafeInputList,
  testProgram
) where

import Polysemy (Members, Sem, reinterpret, run)
import Polysemy.Input (Input(Input), input)
import Polysemy.Output (Output, output, runLazyOutputList)
import Polysemy.State (get, put, runState)

import Pair (Pair(Pair),universe, (!))

runUnsafeInputList :: [i] -> Sem (Input i ': r) a -> Sem r a
-- Just like the normal PolySemy.Input.runInputList, except if there aren't enough inputs that's a runtime error!
runUnsafeInputList is = fmap snd . runState is . reinterpret (\case
    Input -> do ~(s : ss) <- get
                put ss
                pure s
    )

parallelEvaluation :: forall x a r.
                      (Sem r ([Pair x], a) -> Sem '[] ([Pair x], a)) ->
                      Pair (Sem (Output (Pair x) ': Input (Pair x) ': r) a) ->
                      Pair ([Pair x], a)
parallelEvaluation handleRest sems = result
  where withOutputs = runLazyOutputList <$> sems
        result = run . handleRest <$> (runUnsafeInputList <$> (Pair o1s o2s) <*> withOutputs)
        Pair o1s o2s = fst <$> result :: Pair [Pair x]


testProgram :: forall r.
               Members '[Input (Pair String), Output (Pair String)] r =>
               Bool -> Sem r String
testProgram self = do output $ ((ownName ++ " says hi to ") ++) <$> parties
                      Pair m11 m12 <- input
                      let c1 = show $ (length m11) + (length m12)
                      output $ (++ ("; " ++ ownName ++ " got " ++ c1 ++ " characters last turn!")) <$> parties
                      Pair m21 m22 <- input
                      let c2 = show $ (length m21) + (length m22)
                      return $ ownName ++ "successfully got " ++ c2 ++ " characters in second round!"
  where parties = Pair "Party1" "Party2"
        ownName = parties ! self

doParallelRecursion :: IO ()
doParallelRecursion = do print "Attempting..."
                         let results = parallelEvaluation id $ testProgram <$> universe
                         print $ "Results: " ++ (show results)


