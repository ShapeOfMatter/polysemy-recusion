module ForthTests
    ( doRealRecursion,
      transpose
    ) where

-- import Data.List (uncons)
import Polysemy (Members, Sem, reinterpret, run)
import Polysemy.Input (Input(Input), input, runInputConst)
import Polysemy.Output (Output, output, runLazyOutputList)
import Polysemy.State (get, put, runState)

import Pair (Pair(Pair), universe, (!))


testProgram :: Members '[Input Bool, Input (Pair String), Output (Pair String)] r => Sem r String
testProgram = do b :: Bool <- input
                 let name = if b then "P2" else "P1"
                 output $ mkMessages $ "Hi I'm " ++ name ++ "!"
                 i1 <- (! (not b)) <$> input
                 let other = reverse $ take 2 $ reverse $ init i1
                 output $ mkMessages $ "Ok, you're  " ++ other ++ "."
                 i2 <- (! (not b)) <$> input
                 return $ "Success!  (got \"" ++ i2 ++ "\")"
  where mkMessages m = (++ m) <$> (Pair "For P1: " "For P2: ")

doRealRecursion :: IO ()
doRealRecursion = do let programs = mkTest <$> universe
                     print $ "Attempting " ++ (show $ length programs) ++ " testPrograms."
                     let Pair (os1, m1) (os2, m2) = run <$> runParallel id programs
                     print $ "Outputs: " ++ (show $ Pair os1 os2)
                     print $ "Messages: " ++ (show $ Pair m1 m2)
  where mkTest :: Members '[Input (Pair String), Output (Pair String)] r => Bool -> Sem r String
        mkTest b = runInputConst b testProgram

runParallel :: forall x a r.
               (Sem r ([Pair x], a) -> Sem '[] ([Pair x], a)) ->
               Pair (Sem (Input (Pair x) ': Output (Pair x) ': r) a) ->
               Pair (Sem r ([Pair x], a))
runParallel handleRest sems = runLazyOutputList <$> (runComInList <$> (transpose os) <*> sems)
  where os :: Pair [Pair x] = fst <$> run <$> handleRest <$> runParallel handleRest sems
        runComInList inpts = (snd <$>) . (runState inpts) . (reinterpret (\case
            Input -> do ~(s : ss) <- get @[Pair x]
                        put @[Pair x] ss
                        pure s
            ))

transpose :: Pair [Pair a] -> Pair [Pair a]
transpose (Pair b1s b2s) = t' b1s b2s 
  where t' ((Pair a11 a12) : a1s) ((Pair a21 a22) : a2s)
            = Pair ((Pair a11 a21) :) ((Pair a12 a22) :) <*> t' a1s a2s
        t' [] [] = pure []
        -- A runtime error here is ok; I have external guarentees that this won't happen.
        t' _ _ = error "Transposition requires the lists to be of equal length."


{--
transpose :: Pair [Pair a] -> Pair [Pair a]
transpose p = t' b1s b2s 
  where t' (a1 : a1s) (a2 : a2s) = (:) <$> ((!) <$> (Pair a1 a2) <*> universe) <*> t' a1s a2s
        t' [] [] = pure []
        t' _ _ = error "Transposition requires the lists to be of equal length."  -- IDK if it's good to have this?

transpose :: forall a. Pair [Pair a] -> Pair [Pair a]
transpose p = maybe id (:) <$> heads <*> tails
  where maybeHeadsTails = uncons <$> p
        oldHeads@(Pair oldH1 oldH2) :: Pair (Maybe (Pair a)) = fst <$$> maybeHeadsTails
        heads :: Pair (Maybe (Pair a))
        heads = (\mh -> maybe Nothing  () ) <$> oldHeads
        tails = transpose $ maybe [] snd <$> maybeHeadsTails
        (<$$>) :: (aa -> bb) -> Pair (Maybe aa) -> Pair (Maybe bb)
        (<$$>) f = ((f <$>) <$>)
        (<*->) :: Pair (Maybe (aa -> bb)) -> Pair aa -> Pair (Maybe bb)
        (<*->) mfs as = (<*>) <$> mfs <*> (pure <$> as)

--}

{--
transpose :: Pair [Pair a] -> Pair [Pair a]
transpose p = maybe id (:) <$> heads <*> tails
  where maybeHeadsTails = uncons <$> p
        oldHeads :: Pair (Maybe (Pair a))
        oldHeads = fst <$$> maybeHeadsTails
        heads :: Pair (Maybe (Pair a))
        heads = (!) <$$> oldHeads `wtf` (pure <$> universe)
        tails = transpose $ maybe [] snd <$> maybeHeadsTails
        (<$$>) :: (a -> b) -> Pair (Maybe a) -> Pair (Maybe b)
        (<$$>) f = ((f <$>) <$>)
        wtf :: Pair (Maybe (Bool -> )) -> Pair (Pair a) -> Pair (Maybe (Pair b))
        (<*->) mfs as = (<*>) <$> mfs <*> (pure <$> as)
 
 - --}
