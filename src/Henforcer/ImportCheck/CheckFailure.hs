{- |
Module      : Henforcer.ImportCheck.CheckFailure
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
Stability   : experimental
Portability : POSIX
-}
module Henforcer.ImportCheck.CheckFailure
  ( CheckFailure (..)
  , CheckedDependency (..)
  , errorMessagesFromList
  ) where

import qualified Data.List.NonEmpty as NEL

import qualified CompatGHC
import qualified Henforcer.Import as Import
import qualified Henforcer.TreeName as TreeName

data CheckedDependency = CheckedDependency
  { dependencySource :: TreeName.TreeName
  , dependencyTarget :: TreeName.TreeName
  }

data CheckFailure
  = DependencyViolation Import.Import CheckedDependency
  | EncapsulationViolation Import.Import TreeName.TreeName
  | QualificationViolation Import.Import [Import.Scheme]
  | OpenImportViolation (NEL.NonEmpty Import.Import) Import.MaxOpenUnaliasedImportsNat

instance CompatGHC.Outputable CheckFailure where
  ppr cf =
    case cf of
      DependencyViolation i cd -> formatDependencyViolation i cd
      EncapsulationViolation i tn -> formatEncapsulationViolation i tn
      QualificationViolation i s -> formatQualificationViolation i s
      OpenImportViolation i n -> formatOpenImportViolation i n

{- | Convert a list of 'CheckFailure' to 'CompatGHC.Messages' so we can hand off to GHC printing mechanism
 in the plugin.
-}
errorMessagesFromList :: [CheckFailure] -> CompatGHC.Messages CheckFailure
errorMessagesFromList =
  CompatGHC.mkMessagesFromList . fmap mkEnv

checkFailureImport :: CheckFailure -> Import.Import
checkFailureImport (DependencyViolation i _) = i
checkFailureImport (EncapsulationViolation i _) = i
checkFailureImport (QualificationViolation i _) = i
checkFailureImport (OpenImportViolation (i NEL.:| _) _) = i

checkFailureLoc :: CheckFailure -> CompatGHC.SrcSpan
checkFailureLoc = Import.srcLocation . checkFailureImport

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic CheckFailure where
  diagnosticMessage = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const []

mkEnv :: CheckFailure -> CompatGHC.MsgEnvelope CheckFailure
mkEnv cf =
  CompatGHC.mkErrorMsgEnvelope (checkFailureLoc cf) cf

formatDependencyViolation :: Import.Import -> CheckedDependency -> CompatGHC.SDoc
formatDependencyViolation imp dep =
  CompatGHC.sep
    [ CompatGHC.hsep
        [ formatImportSubject imp
        , CompatGHC.text "is forbidden by the declaration that the module tree"
        , CompatGHC.text (TreeName.format $ dependencySource dep)
        , CompatGHC.text "depends on"
        , CompatGHC.text (TreeName.format $ dependencyTarget dep)
        ]
    , CompatGHC.blankLine
    ]

formatEncapsulationViolation :: Import.Import -> TreeName.TreeName -> CompatGHC.SDoc
formatEncapsulationViolation imp treeName =
  CompatGHC.sep
    [ CompatGHC.sep
        [ formatImportSubject imp
        , CompatGHC.text "is forbidden because it is an internal module of the encapsulated tree"
        , CompatGHC.text (TreeName.format treeName)
        ]
    , CompatGHC.blankLine
    ]

formatImportSubject :: Import.Import -> CompatGHC.SDoc
formatImportSubject imp =
  CompatGHC.hsep
    [ CompatGHC.text "The import of"
    , CompatGHC.text (CompatGHC.moduleNameString (Import.importedModule imp))
    ]

formatQualificationViolation :: Import.Import -> [Import.Scheme] -> CompatGHC.SDoc
formatQualificationViolation imp alloweds =
  let rebuildFromImport =
        rebuildImportStatementFromScheme (Import.importedModule imp)
      beginningDoc =
        CompatGHC.hang
          ( CompatGHC.sep
              [ CompatGHC.hsep
                  [ formatImportSubject imp
                  , CompatGHC.text "is improper because it does not match one of the allowed import schemes."
                  ]
              , CompatGHC.cat
                  [ CompatGHC.text "It was imported"
                  , CompatGHC.colon
                  ]
              ]
          )
          4
          (rebuildFromImport . Import.buildScheme . CompatGHC.unLoc $ Import.importDecl imp)
      endDoc =
        CompatGHC.hang
          ( CompatGHC.cat
              [ CompatGHC.text "But it may only be imported in the followings ways"
              , CompatGHC.colon
              ]
          )
          4
          (CompatGHC.vcat $ fmap rebuildFromImport alloweds)
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        , endDoc
        , CompatGHC.blankLine
        ]

formatOpenImportViolation ::
  NEL.NonEmpty Import.Import -> Import.MaxOpenUnaliasedImportsNat -> CompatGHC.SDoc
formatOpenImportViolation imps maxAllowedNat =
  let rebuildFromImport imp =
        rebuildImportStatementFromScheme (Import.importedModule imp) . Import.buildScheme . CompatGHC.unLoc $
          Import.importDecl imp
      beginningDoc =
        CompatGHC.hang
          ( CompatGHC.sep
              [ CompatGHC.hsep
                  [ CompatGHC.text "There were too many open imports. The max allowed is:"
                  , CompatGHC.text (Import.showMaxOpenUnaliasedImportsNat maxAllowedNat)
                  ]
              , CompatGHC.cat
                  [ CompatGHC.text "The open imports are"
                  , CompatGHC.colon
                  ]
              ]
          )
          4
          (CompatGHC.sep . fmap rebuildFromImport $ NEL.toList imps)
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

{- | Rebuild the part of an import statement we subject to rules, namely everything up to explicit
 items imported or hiding
-}
rebuildImportStatementFromScheme :: CompatGHC.ModuleName -> Import.Scheme -> CompatGHC.SDoc
rebuildImportStatementFromScheme modName schema =
  CompatGHC.hsep
    [ CompatGHC.dot
    , CompatGHC.hsep
        [ CompatGHC.text "import"
        , case Import.safe schema of
            Import.WithoutSafe -> CompatGHC.empty -- Purposely do not mention the safe keyword when not forced, as
            -- this makes the output needlessly complex and we don't need to
            -- mention something requiring an extension if users didn't ask for
            -- it explicitly.
            Import.WithSafe -> CompatGHC.text "safe"
        , case Import.qualification schema of
            CompatGHC.QualifiedPre -> CompatGHC.text "qualified"
            CompatGHC.QualifiedPost -> CompatGHC.empty
            CompatGHC.NotQualified -> CompatGHC.empty
        , CompatGHC.text (CompatGHC.moduleNameString modName)
        , case Import.qualification schema of
            CompatGHC.QualifiedPre -> CompatGHC.empty
            CompatGHC.QualifiedPost -> CompatGHC.text "qualifed"
            CompatGHC.NotQualified -> CompatGHC.empty
        , case Import.alias schema of
            Import.WithoutAlias ->
              CompatGHC.empty
            Import.WithAlias name ->
              CompatGHC.hsep
                [ CompatGHC.text "as"
                , CompatGHC.text (CompatGHC.moduleNameString name)
                ]
        ]
    ]
