{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning where

import Data.Aeson ((.=))
import Data.Text (Text)
import qualified Data.Aeson as Json

import Data.Monoid (mempty)
import Data.String
import Reporting.Helpers ((<>), text)
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Helpers as Help
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report



-- ALL POSSIBLE WARNINGS


data Warning
    = UnusedImport ModuleName.Raw
    | MissingTypeAnnotation Text Type.Canonical
    | TopLevelBinding Text
    | ExternalBinding Text String



-- TO REPORT


toReport :: RenderType.Localizer -> Warning -> Report.Report
toReport localizer warning =
  case warning of
    UnusedImport moduleName ->
        Report.report
          "unused import"
          Nothing
          ("Module `" <> ModuleName.toText moduleName <> "` is unused.")
          (text "Best to remove it. Don't save code quality for later!")
    TopLevelBinding name ->
        Report.report
          "toplevel value"
          Nothing
          ("Top-level value " <> Help.functionName name <> " exists!!")
          ( mempty
          )
    ExternalBinding name mod ->
        Report.report
          "external value"
          Nothing
          ("External value " <> Help.functionName name <> " exists!!" <> fromString mod)
          ( mempty
          )
    MissingTypeAnnotation name inferredType ->
        Report.report
          "missing type annotation"
          Nothing
          ("Top-level value " <> Help.functionName name <> " does not have a type annotation.")
          ( Help.stack
              [ text "I inferred the type annotation so you can copy it into your code:"
              , RenderType.annotation localizer name inferredType
              ]
          )



-- TO JSON


toJson :: RenderType.Localizer -> FilePath -> A.Located Warning -> Json.Value
toJson localizer filePath (A.A region warning) =
  let
    (maybeRegion, additionalFields) =
        Report.toJson [] (toReport localizer warning)
  in
      Json.object $
        [ "file" .= filePath
        , "region" .= maybe region id maybeRegion
        , "type" .= ("warning" :: Text)
        ]
        ++ additionalFields
