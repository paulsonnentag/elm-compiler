{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Nitpick.TopLevelTypes (topLevelTypes) where

import Data.Map ((!))
import Data.Text (Text)
import qualified Data.Foldable as F
import qualified Data.Map as Map

import qualified AST.Expression.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Canonicalize.Effects as Effects
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning


type Result =
  Result.Result () Warning.Warning Error.Error


type TypeDict =
  Map.Map Text Type.Canonical



-- CHECK TOP LEVEL TYPES


topLevelTypes :: TypeDict -> Can.SortedDefs -> Result [Can.Def]
topLevelTypes typeEnv sortedDefs =
  let
    check x =
      do checkAnnotation2 x
         checkAnnotation typeEnv x
  in
    case sortedDefs of
      Can.NoMain defs ->
        pure defs
          <* F.traverse_ check defs

      Can.YesMain before main after ->
        do  check main
              <* F.traverse_ check before
              <* F.traverse_ check after

            mainDef <- checkMain typeEnv main

            return (before ++ mainDef : after)


checkAnnotation2 :: Can.Def -> Result ()
checkAnnotation2 (Can.Def _ (A.A region pat) bdy maybeType) =
  do checkForVar bdy
     case pat of
       (P.Var name) ->
           let warning = Warning.TopLevelBinding name
           in Result.warn region warning ()
       _ ->
           return ()

checkForVar :: Can.Expr -> Result ()
checkForVar (A.A region qqqq) =
    case qqqq of
      Can.Literal _ -> pure ()
      Can.Var x -> varHelper x region
      Can.List xs -> mapM_ checkForVar xs
      Can.Binop v e1 e2 ->
          do varHelper v region
             checkForVar e1
             checkForVar e2
      Can.Lambda _ e -> checkForVar e
      Can.App e1 e2 ->
          do checkForVar e1
             checkForVar e2
      Can.If exprs e ->
          do mapM_ (\(e1, e2) -> checkForVar e1 >> checkForVar e2) exprs
             checkForVar e
      Can.Let _ e -> checkForVar e
      Can.Case e exprs ->
          do mapM_ (\(_, e1) -> checkForVar e1) exprs
             checkForVar e
      Can.Ctor v exprs ->
          do varHelper v region
             mapM_ checkForVar exprs
      Can.Access x _ -> checkForVar x
      Can.Update e exprs ->
          do checkForVar e
             mapM_ (\(_, e1) -> checkForVar e1) exprs
      Can.Record exprs ->
          mapM_ (\(_, e1) -> checkForVar e1) exprs
      _ -> pure ()

varHelper :: Var.Canonical -> R.Region -> Result ()
varHelper x region =
    case Var._home x of
        Var.Module mn ->
            let warning =
                    Warning.ExternalBinding (Var._name x) (show mn)
            in Result.warn region warning ()
        _ -> pure ()

-- MISSING ANNOTATIONS

checkAnnotation :: TypeDict -> Can.Def -> Result ()
checkAnnotation typeEnv (Can.Def _ (A.A region pattern) _ maybeType) =
  case (pattern, maybeType) of
    (P.Var name, Nothing) ->
      let
        warning =
          Warning.MissingTypeAnnotation name (typeEnv ! name)
      in
        Result.warn region warning ()

    _ ->
      return ()



-- CHECK MAIN TYPE


checkMain :: TypeDict -> Can.Def -> Result Can.Def
checkMain typeEnv (Can.Def facts pattern@(A.A region _) body maybeType) =
  let
    mainType =
      typeEnv ! "main"

    makeError tipe maybeMsg =
      A.A region (Error.BadFlags tipe maybeMsg)

    getMainKind =
      case Type.deepDealias mainType of
        Type.App name [_]
          | name == vdomNode ->
              return Can.VDom

        Type.App name [flags, _, _]
          | name == program && flags == never ->
              return Can.NoFlags

          | name == program ->
              return (Can.Flags flags)
                <* Effects.checkPortType makeError flags

        _ ->
          Result.throw region (Error.BadMain mainType)
  in
    do  kind <- getMainKind
        let newBody = A.A undefined (Can.Program kind body)
        return (Can.Def facts pattern newBody maybeType)


program :: Type.Canonical
program =
  Type.Type (Var.inCore "Platform" "Program")


never :: Type.Canonical
never =
  Type.Type (Var.inCore "Basics" "Never")


vdomNode :: Type.Canonical
vdomNode =
  let
    vdom =
      ModuleName.Canonical Pkg.virtualDom "VirtualDom"
  in
    Type.Type (Var.fromModule vdom "Node")
