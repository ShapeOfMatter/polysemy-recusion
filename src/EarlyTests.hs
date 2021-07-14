module EarlyTests
    ( testUndefinedInput,
      doUndefinedInput,
      doDummyRecursion,
      doRealRecursion
    ) where

import Polysemy (Member, Members, Sem, reinterpret, run)
import Polysemy.Input (Input(Input), input, runInputList)
import Polysemy.Output (Output, output, runLazyOutputList)
import Polysemy.State (get, put, runState)

testUndefinedInput :: Member (Input (Maybe String)) r => Sem r String
testUndefinedInput = return "Success!"

doUndefinedInput :: IO ()
doUndefinedInput = do print "Attempting testUndefinedInput..."
                      let message = run $ (runInputList undefined) $ testUndefinedInput
                      print $ "Got message: " ++ message

testDummyRecursion :: Members '[Input String, Output String] r => Sem r String
testDummyRecursion = return "Success!"

doDummyRecursion :: IO ()
doDummyRecursion = do print "Attempting testDummyRecursion..."
                      let ([], message) = run $ inOutRecursion id testDummyRecursion
                      print $ "Got message: " ++ message

testRealRecursion :: Members '[Input String, Output String] r => Sem r String
testRealRecursion = do output "?"
                       i1 <- input
                       output $ "Success" ++ i1
                       i2 <- input
                       return $ (init i2) ++ "!"

doRealRecursion :: IO ()
doRealRecursion = do print "Attempting testRealRecursion..."
                     let (outpts, message) = run $ inOutRecursion id testRealRecursion
                     print $ "Output: " ++ (show outpts)
                     print $ "Got message: " ++ message

inOutRecursion :: forall x a r.
                  (Sem r ([x], a) -> Sem '[] ([x], a)) -> Sem (Input x ': Output x ': r) a -> Sem r ([x], a)
inOutRecursion handleRest sem = runLazyOutputList $ (runComInList outpt) $ sem
  where outpt :: [x]
        outpt = fst $ run $ handleRest $ inOutRecursion handleRest sem
        runComInList inpts = (snd <$>) . (runState inpts) . (reinterpret (\case
            Input -> do ~(s : ss) <- get @[x]
                        put @[x] ss
                        pure s
            ))



