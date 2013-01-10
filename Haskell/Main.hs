module Main (main) where

import qualified Control.Monad.Identity as MTL
import qualified Control.Monad.Error as MTL
import qualified Control.Monad.State.Strict as MTL
import qualified Language.Haskell.Exts as HS
import qualified System.Environment as IO


main :: IO ()
main = do
  parameters <- IO.getArgs
  case parameters of
    [inputPath, outputPath] -> do
      convertFile inputPath outputPath
    _ -> do
      putStrLn $ "Usage: haskell-to-idris input.hs output.idr"


convertFile :: FilePath -> FilePath -> IO ()
convertFile inputPath outputPath = do
  putStrLn $ "Converting " ++ inputPath ++ " to " ++ outputPath ++ "."
  let parseMode = HS.defaultParseMode {
                      HS.parseFilename = inputPath
                    }
  parseResult <- HS.parseFileWithComments parseMode inputPath
  case parseResult of
    HS.ParseFailed location headline -> do
      putStrLn $ "Parse error at line " ++ (show $ HS.srcLine location)
                 ++ ": " ++ headline
    HS.ParseOk (module', comments) -> do
      case runConversion $ visitModule module' comments of
        Left problems -> do
          mapM_ (\(maybeLocation, headline) -> do
                   let report = (maybe "" (\location ->
                                             "Line "
                                             ++ (show $ HS.srcLine location)
                                             ++ ": ") maybeLocation)
                                ++ headline
                   putStrLn report)
                problems
          putStrLn $ "Not converted."
        Right output -> do
          writeFile outputPath output
          putStrLn $ "Converted."


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


visitModule :: HS.Module -> [HS.Comment] -> Conversion ()
visitModule
    (HS.Module location name _ _ exports imports declarations) comments = do
  HS.ModuleName name <- return name
  emit name

