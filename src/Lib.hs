module Lib
    ( testUndefinedInput,
      doUndefinedInput,
      doDummyRecursion,
      doRealRecursion
    ) where

import Polysemy (Member, Members, Sem, run)
import Polysemy.Input (Input, input, runInputList)
import Polysemy.Output (Output, output, runLazyOutputList)

testUndefinedInput :: Member (Input (Maybe String)) r => Sem r String
testUndefinedInput = return "Success!"

doUndefinedInput :: IO ()
doUndefinedInput = do print "Attempting testUndefinedInput..."
                      let message = run $ (runInputList undefined) $ testUndefinedInput
                      print $ "Got message: " ++ message

testDummyRecursion :: Members '[Input (Maybe String), Output String] r => Sem r String
testDummyRecursion = return "Success!"

doDummyRecursion :: IO ()
doDummyRecursion = do print "Attempting testDummyRecursion..."
                      let ([], message) = run $ inOutRecursion id testDummyRecursion
                      print $ "Got message: " ++ message

testRealRecursion :: Members '[Input (Maybe String), Output String] r => Sem r String
testRealRecursion = do output "Success?"
                       mResult <- input
                       let result = maybe "Failure!" id mResult
                       return $ (init result) ++ "!"

doRealRecursion :: IO ()
doRealRecursion = do print "Attempting testRealRecursion..."
                     let (outpts, message) = run $ inOutRecursion id testRealRecursion
                     print $ "Output: " ++ (show outpts)
                     print $ "Got message: " ++ message

inOutRecursion :: forall x a r.
                  (Sem r ([x], a) -> Sem '[] ([x], a)) -> Sem (Input (Maybe x) ': Output x ': r) a -> Sem r ([x], a)
inOutRecursion handleRest sem = (runLazyOutputList) $ (runInputList outpt) $ sem
  where outpt :: [x]
        outpt = fst $ run $ handleRest $ inOutRecursion handleRest sem


