module Main (main) where

import qualified Language.Haskell.Exts as HS


main :: IO ()
main = do
  parameters <- getArgs
  case parameters of
    [inputPath, outputPath] -> do
      convertFile inputPath outputPath


convertFile :: FilePath -> FilePath -> IO ()
convertFile inputPath outputPath = do
  putStrLn $ "Converting " ++ inputPath ++ " to " ++ outputPath ++ "."
  let parseMode = HS.defaultParseMode {
		      HS.parseFilename = inputPath
		    }
  (parseResult, comments) <- HS.parseFileWithComments parseMode inputPath
  case parseResult of
    HS.ParseFailed location headline -> do
      putStrLn $ "  Parse error at line " ++ HS.srcLine ++ ": " ++ headline
    HS.ParseOk module -> do
      putStrLn $ "  Converted."

