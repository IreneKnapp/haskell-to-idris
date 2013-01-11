module Idris.Types
  (Emittable(..),
   Name(..),
   Module(..),
   TopDeclaration(..),
   TypeVariableBinding(..),
   TypeSignature(..))
  where

import Conversion


class Emittable node where
  emitNode :: node -> Conversion ()


data Name = Name String
instance Emittable Name where
  emitNode (Name string) = emit string


data Module =
  Module {
      moduleName :: Name,
      moduleDeclarations :: [TopDeclaration]
    }
instance Emittable Module where
  emitNode module' = do
    emit "module "
    emitNode $ moduleName module'
    emit " where\n\n\n"
    mapM_ (\declaration -> do
             emitNode declaration
             emit "\n\n")
          (moduleDeclarations module')


data TopDeclaration
  = ClassTopDeclaration {
        classTopDeclarationName :: Name,
        classTopDeclarationTypeVariableBindings :: [TypeVariableBinding]
      }
instance Emittable TopDeclaration where
  emitNode declaration = do
    emit "class "
    emitNode $ classTopDeclarationName declaration
    mapM_ (\binding -> do
             emit " "
             emitNode binding)
          (classTopDeclarationTypeVariableBindings declaration)
    emit " where\n"


data TypeVariableBinding
  = TypeVariableBinding {
        typeVariableBindingName :: Name,
        typeVariableBindingSignature :: Maybe TypeSignature
      }
instance Emittable TypeVariableBinding where
  emitNode binding = do
    case typeVariableBindingSignature binding of
      Nothing -> do
        emitNode $ typeVariableBindingName binding
      Just signature -> do
        emit "("
        emitNode $ typeVariableBindingName binding
        emit " : "
        emitNode signature
        emit ")"


data TypeSignature
  = VariableTypeSignature Name
  | FunctionTypeSignature TypeSignature TypeSignature
  | ParenthesizedTypeSignature TypeSignature
instance Emittable TypeSignature where
  emitNode (VariableTypeSignature name) = emitNode name
  emitNode (FunctionTypeSignature lhs rhs) = do
    emitNode lhs
    emit " -> "
    emitNode rhs
  emitNode (ParenthesizedTypeSignature signature) = do
    emit "("
    emitNode signature
    emit ")"

