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
  emit $ "Module " ++ name ++ "\n"
  emit "\n"
  mapM_ visitDeclaration declarations


emitName :: HS.Name -> Conversion ()
emitName (HS.Ident name) = emit name
emitName (HS.Symbol name) = emit name


emitModuleName :: HS.ModuleName -> Conversion ()
emitModuleName (HS.ModuleName name) = emit name


emitQName :: HS.QName -> Conversion ()
emitQName (HS.Qual moduleName name) = do
  emitModuleName moduleName
  emit "."
  emitName name
emitQName (HS.UnQual name) = emitName name
emitQName (HS.Special HS.UnitCon) = emit "()"
emitQName (HS.Special HS.ListCon) = emit "[]"
emitQName (HS.Special HS.FunCon) = emit "->"
emitQName (HS.Special (HS.TupleCon _ n)) = do
  emit "("
  mapM_ (\_ -> emit ",") [0 .. n - 2]
  emit ")"
emitQName (HS.Special HS.Cons) = emit "::"
emitQName (HS.Special HS.UnboxedSingleCon) = emit "()"


visitDeclaration :: HS.Decl -> Conversion ()

visitDeclaration
    (HS.TypeDecl location name _ _) = do
  emitName name
  emit " : Type\n"
  emit "\n"

visitDeclaration
    (HS.TypeFamDecl location name _ _) = do
  emitName name
  emit " ... : Type\n"
  emit "\n"

visitDeclaration
    (HS.DataDecl location _ _ name _ _ _) = do
  emit "data "
  emitName name
  emit "\n"
  emit "  = ...\n"
  emit "\n"

visitDeclaration
    (HS.GDataDecl location _ _ name _ _ _ _) = do
  emit "data "
  emitName name
  emit "\n"
  emit "  = ... : Type\n"
  emit "\n"

visitDeclaration
    (HS.DataFamDecl location _ name _ _) = do
  emit "data "
  emitName name
  emit "\n"
  emit "  = ... : ... -> Type\n"
  emit "\n"

visitDeclaration
    (HS.TypeInsDecl location _ _) = do
  return ()

visitDeclaration
    (HS.DataInsDecl location _ _ _ _) = do
  return ()

visitDeclaration
    (HS.GDataInsDecl location _ _ _ _ _) = do
  return ()

visitDeclaration
    (HS.ClassDecl location _ name _ _ _) = do
  emit "class "
  emitName name
  emit " where\n"
  emit "\n"

visitDeclaration
    (HS.InstDecl location _ qname _ _) = do
  return ()

visitDeclaration
    (HS.DerivDecl location _ qname _) = do
  return ()

visitDeclaration
    (HS.InfixDecl location _ _ _) = do
  return ()

visitDeclaration
    (HS.DefaultDecl location _) = do
  return ()

visitDeclaration
    (HS.SpliceDecl location _) = do
  return ()

visitDeclaration
    (HS.TypeSig location _ _) = do
  return ()

visitDeclaration
    (HS.FunBind _) = do
  return ()

visitDeclaration
    (HS.PatBind location _ _ _ _) = do
  return ()

visitDeclaration
    (HS.ForImp location _ _ _ _ _) = do
  return ()

visitDeclaration
    (HS.ForExp location _ _ _ _) = do
  return ()

visitDeclaration
    (HS.RulePragmaDecl location _) = do
  return ()

visitDeclaration
    (HS.DeprPragmaDecl location _) = do
  return ()

visitDeclaration
    (HS.WarnPragmaDecl location _) = do
  return ()

visitDeclaration
    (HS.InlineSig location _ _ qname) = do
  return ()

visitDeclaration
    (HS.InlineConlikeSig location _ qname) = do
  return ()

visitDeclaration
    (HS.SpecSig location qname _) = do
  return ()

visitDeclaration
    (HS.SpecInlineSig location _ _ qname _) = do
  return ()

visitDeclaration
    (HS.InstSig location _ qname _) = do
  return ()

visitDeclaration
    (HS.AnnPragma location _) = do
  return ()

