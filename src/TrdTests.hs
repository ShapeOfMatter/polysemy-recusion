module TrdTests (
  inptsTake,
  doParallelRecursion,
  outptsHead,
  parallelEvaluation
) where

import Polysemy (Sem, run)
import Polysemy.Input (Input)
import Polysemy.Output (Output, runLazyOutputList)

import Pair (Pair(Pair), universe)
import SndTests (runUnsafeInputList, testProgram)

outptsHead :: forall x a r.
              (Sem r ([Pair x], a) -> Sem '[] ([Pair x], a)) ->
              Pair (Sem (Output (Pair x) ': Input (Pair x) ': r) a) ->
              Pair (Pair x)
outptsHead handleRest sems = head <$> fst <$> evaluation
  where withOutpts = runLazyOutputList <$> sems
        runInpts = runUnsafeInputList <$> pure []
        evaluation = run . handleRest <$> (runInpts <*> withOutpts)

inptsTake :: forall x a r.
             (Sem r ([Pair x], a) -> Sem '[] ([Pair x], a)) ->
             Pair (Sem (Output (Pair x) ': Input (Pair x) ': r) a) ->
             Pair [Pair x] ->
             Pair [Pair x]
inptsTake handleRest sems inpts = if n > minLen newInpts then newInpts else inptsTake handleRest sems newInpts
  where minLen = minimum . (length <$>)
        n = (minLen inpts) + 1
        withOutpts = runLazyOutputList <$> sems
        evaluation = run . handleRest <$> (runUnsafeInputList <$> inpts <*> withOutpts)
        Pair o1s o2s = fst <$> evaluation
        messagePassing (Pair o11 o12) (Pair o21 o22) = (Pair o11 o21, Pair o12 o22)
        messagesPassed = zipWith messagePassing o1s o2s
        newInpts = take n <$> Pair (fst <$> messagesPassed) (snd <$> messagesPassed)

parallelEvaluation :: forall x a r.
                      (Sem r ([Pair x], a) -> Sem '[] ([Pair x], a)) ->
                      Pair (Sem (Output (Pair x) ': Input (Pair x) ': r) a) ->
                      Pair ([Pair x], a)
parallelEvaluation handleRest sems = run . handleRest <$> (runUnsafeInputList <$> inpts <*> withOutputs)
  where withOutputs = runLazyOutputList <$> sems
        inpts = inptsTake handleRest sems $ pure []

doParallelRecursion :: IO ()
doParallelRecursion = do print "Attempting..."
                         let results = parallelEvaluation id $ testProgram <$> universe
                         print $ "Results: " ++ (show results)


