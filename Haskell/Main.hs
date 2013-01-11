module Main (main) where

import Data.Maybe

import qualified Language.Haskell.Exts as HS
import qualified System.Environment as IO

import qualified Idris.Types as IDR

import Conversion


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
      case runConversion $ convertModule module' comments of
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


class HasLocation node where
  nodeLocation :: node -> Maybe HS.SrcLoc
instance HasLocation HS.Decl where
  nodeLocation (HS.TypeDecl location _ _ _) = Just location
  nodeLocation (HS.TypeFamDecl location _ _ _) = Just location
  nodeLocation (HS.DataDecl location _ _ _ _ _ _) = Just location
  nodeLocation (HS.GDataDecl location _ _ _ _ _ _ _) = Just location
  nodeLocation (HS.DataFamDecl location _ _ _ _) = Just location
  nodeLocation (HS.TypeInsDecl location _ _) = Just location
  nodeLocation (HS.DataInsDecl location _ _ _ _) = Just location
  nodeLocation (HS.GDataInsDecl location _ _ _ _ _) = Just location
  nodeLocation (HS.ClassDecl location _ _ _ _ _) = Just location
  nodeLocation (HS.InstDecl location _ _ _ _) = Just location
  nodeLocation (HS.DerivDecl location _ _ _) = Just location
  nodeLocation (HS.InfixDecl location _ _ _) = Just location
  nodeLocation (HS.DefaultDecl location _) = Just location
  nodeLocation (HS.SpliceDecl location _) = Just location
  nodeLocation (HS.TypeSig location _ _) = Just location
  nodeLocation (HS.FunBind matches) =
    listToMaybe $ catMaybes $ map nodeLocation matches
  nodeLocation (HS.PatBind location _ _ _ _) = Just location
  nodeLocation (HS.ForImp location _ _ _ _ _) = Just location
  nodeLocation (HS.ForExp location _ _ _ _) = Just location
  nodeLocation (HS.RulePragmaDecl location _) = Just location
  nodeLocation (HS.DeprPragmaDecl location _) = Just location
  nodeLocation (HS.WarnPragmaDecl location _) = Just location
  nodeLocation (HS.InlineSig location _ _ _) = Just location
  nodeLocation (HS.InlineConlikeSig location _ _) = Just location
  nodeLocation (HS.SpecSig location qname _) = Just location
  nodeLocation (HS.SpecInlineSig location _ _ _ _) = Just location
  nodeLocation (HS.InstSig location _ _ _) = Just location
  nodeLocation (HS.AnnPragma location _) = Just location
instance HasLocation HS.Match where
  nodeLocation (HS.Match location _ _ _ _ _) = Just location


convertModule :: HS.Module -> [HS.Comment] -> Conversion ()
convertModule module' comments = do
  idrisModule <- visitModule module' comments
  IDR.emitNode idrisModule


visitName :: HS.Name -> Conversion IDR.Name
visitName (HS.Ident name) = return $ IDR.Name name
visitName (HS.Symbol name) = return $ IDR.Name name


visitModuleName :: HS.ModuleName -> Conversion IDR.Name
visitModuleName (HS.ModuleName name) = return $ IDR.Name name


{-
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
-}


visitModule :: HS.Module -> [HS.Comment] -> Conversion IDR.Module
visitModule
    (HS.Module location name _ _ exports imports declarations) comments = do
  idrisName <- visitModuleName name
  idrisDeclarations <- mapM visitTopDeclaration declarations
                       >>= return . concat
  return IDR.Module {
             IDR.moduleName = idrisName,
             IDR.moduleDeclarations = idrisDeclarations
           }


visitTopDeclaration :: HS.Decl -> Conversion [IDR.TopDeclaration]

visitTopDeclaration
    (HS.ClassDecl location _ name bindings _ declarations) = do
  idrisName <- visitName name
  idrisBindings <- mapM visitTypeVariableBinding bindings
  let idrisClass = IDR.ClassTopDeclaration {
                       IDR.classTopDeclarationName = idrisName,
                       IDR.classTopDeclarationTypeVariableBindings =
                         idrisBindings
                     }
  return [idrisClass]

visitTopDeclaration declaration = do
  problem (nodeLocation declaration) "Unimplemented."
  return []


visitTypeVariableBinding :: HS.TyVarBind -> Conversion IDR.TypeVariableBinding
visitTypeVariableBinding (HS.KindedVar name kind) = do
  idrisName <- visitName name
  idrisKind <- visitKindSignature kind
  return IDR.TypeVariableBinding {
             IDR.typeVariableBindingName = idrisName,
             IDR.typeVariableBindingSignature = Just idrisKind
           }
visitTypeVariableBinding (HS.UnkindedVar name) = do
  idrisName <- visitName name
  return IDR.TypeVariableBinding {
             IDR.typeVariableBindingName = idrisName,
             IDR.typeVariableBindingSignature = Nothing
           }


visitKindSignature :: HS.Kind -> Conversion IDR.TypeSignature
visitKindSignature HS.KindStar = do
  return $ IDR.VariableTypeSignature $ IDR.Name "Type"
visitKindSignature HS.KindBang = do
  return $ IDR.VariableTypeSignature $ IDR.Name "Type"
visitKindSignature (HS.KindFn lhs rhs) = do
  idrisLHS <- visitKindSignature lhs
  idrisRHS <- visitKindSignature rhs
  return $ IDR.FunctionTypeSignature idrisLHS idrisRHS
visitKindSignature (HS.KindParen kind) = do
  idrisKind <- visitKindSignature kind
  return $ IDR.ParenthesizedTypeSignature idrisKind
visitKindSignature (HS.KindVar name) = do
  idrisName <- visitName name
  return $ IDR.VariableTypeSignature idrisName


visitClassDeclaration :: HS.ClassDecl -> Conversion ()
visitClassDeclaration (HS.ClsDecl declaration) = emit "hm\n"
visitClassDeclaration _ = return ()

