module Conversion
  (Problem(..),
   Conversion(..),
   runConversion,
   abort,
   problem,
   emit)
  where

import qualified Control.Monad.Identity as MTL
import qualified Control.Monad.Error as MTL
import qualified Control.Monad.State.Strict as MTL
import qualified Language.Haskell.Exts as HS


type Problem = (Maybe HS.SrcLoc, String)

instance MTL.Error () where
  noMsg = ()


data Conversion a =
  Conversion (MTL.ErrorT () (MTL.StateT ([Problem], [String]) MTL.Identity) a)

instance Monad Conversion where
  return a = Conversion $ return a
  (Conversion a) >>= b = Conversion $ do
    v <- a
    case b v of
      Conversion b -> b


runConversion :: Conversion () -> Either [Problem] String
runConversion (Conversion action) =
  MTL.runIdentity $ do
    flip MTL.evalStateT ([], []) $ do
      eitherProblemsResult <- MTL.runErrorT action
      case eitherProblemsResult of
        Left () -> do
          (problems, _) <- MTL.get
          return $ Left problems
        Right () -> do
          (_, outputItems) <- MTL.get
          return $ Right $ concat outputItems


abort :: Conversion a
abort = Conversion $ do
  MTL.throwError ()


problem :: Maybe HS.SrcLoc -> String -> Conversion ()
problem maybeLocation headline = Conversion $ do
  (problems, output) <- MTL.get
  let problems' = problems ++ [(maybeLocation, headline)]
  MTL.put (problems', output)


emit :: String -> Conversion ()
emit string = Conversion $ do
  (problems, output) <- MTL.get
  let output' = output ++ [string]
  MTL.put (problems, output')
