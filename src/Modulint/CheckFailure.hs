module Modulint.CheckFailure
  (CheckFailure(..)
  , CheckedDependency(..)
  , errorMessagesFromList
  , toSDoc
  )
where

import qualified CompatGHC as GHC
import qualified Modulint.Import as Import
import qualified Modulint.Scheme as Scheme
import qualified Modulint.TreeName as TreeName

data CheckedDependency =
  CheckedDependency
    { dependencySource :: TreeName.TreeName
    , dependencyTarget :: TreeName.TreeName
    }

data CheckFailure
  = DependencyViolation    Import.Import CheckedDependency
  | EncapsulationViolation Import.Import TreeName.TreeName
  | QualificationViolation Import.Import [Scheme.Scheme]

instance GHC.Outputable CheckFailure where
  ppr cf =
    case cf of
      DependencyViolation i cd -> formatDependencyViolation i cd
      EncapsulationViolation i tn -> formatEncapsulationViolation i tn
      QualificationViolation i s -> formatQualificationViolation i s

-- | Convert a list of 'CheckFailure' to 'GHC.Messages' so we can hand off to GHC printing mechanism
-- in the plugin.
errorMessagesFromList :: [CheckFailure] -> GHC.Messages CheckFailure
errorMessagesFromList =
  GHC.mkMessagesFromList . fmap mkEnv

-- | toSDoc performs a conversion to a printable document type. This includes hanging the placement
-- information because when running as a standalone executable, we do not have a ghc to do that for
-- us.
toSDoc :: CheckFailure -> GHC.SDoc
toSDoc cf =
  hangImportLoc (checkFailureImport cf) $ GHC.ppr cf


checkFailureImport :: CheckFailure -> Import.Import
checkFailureImport (DependencyViolation i _) = i
checkFailureImport (EncapsulationViolation i _) = i
checkFailureImport (QualificationViolation i _) = i

checkFailureLoc :: CheckFailure -> GHC.SrcSpan
checkFailureLoc = Import.srcLocation . checkFailureImport

-- | The only part of the 'GHC.Diagnostic' class that we really care about is the
-- 'diagnosticMessage', used for printing.
instance GHC.Diagnostic CheckFailure where
  diagnosticMessage = GHC.mkSimpleDecorated . GHC.ppr
  diagnosticReason = const GHC.ErrorWithoutFlag
  diagnosticHints = const []

mkEnv :: CheckFailure -> GHC.MsgEnvelope CheckFailure
mkEnv cf =
  GHC.mkErrorMsgEnvelope (checkFailureLoc cf) cf

hangImportLoc :: Import.Import -> GHC.SDoc -> GHC.SDoc
hangImportLoc imp =
  GHC.hang
    ( GHC.hcat
      [ GHC.ppr (Import.srcLocation imp)
      , GHC.colon
      ]
    )
    4

formatDependencyViolation :: Import.Import -> CheckedDependency -> GHC.SDoc
formatDependencyViolation imp dep =
  GHC.sep
    [ GHC.hsep
      [ formatImportSubject imp
      , GHC.text "is forbidden by the declaration that the module tree"
      , GHC.text (TreeName.format $ dependencySource dep)
      , GHC.text "depends on"
      , GHC.text (TreeName.format $ dependencyTarget dep)
      ]
    , GHC.blankLine
    ]

formatEncapsulationViolation :: Import.Import -> TreeName.TreeName -> GHC.SDoc
formatEncapsulationViolation imp treeName =
  GHC.sep
    [ GHC.sep
      [ formatImportSubject imp
      , GHC.text "is forbidden because it is an internal module of the encapsulated tree"
      , GHC.text (TreeName.format treeName)
      ]
    , GHC.blankLine
    ]

formatImportSubject :: Import.Import -> GHC.SDoc
formatImportSubject imp =
  GHC.hsep
    [ GHC.text "The import of"
    , GHC.text (GHC.moduleNameString (Import.importedModule imp))
    ]

formatQualificationViolation :: Import.Import -> [Scheme.Scheme] -> GHC.SDoc
formatQualificationViolation imp alloweds =
  let
    rebuildFromImport =
      rebuildImportStatementFromScheme (Import.importedModule imp)
    beginningDoc =
      GHC.hang
        (GHC.sep
         [ GHC.hsep [ formatImportSubject imp
                   , GHC.text "is improper because it does not match one of the allowed import schemes."
                   ]
         , GHC.cat [ GHC.text "It was imported"
                    , GHC.colon
                    ]
         ]
        )
        4
        (rebuildFromImport . Scheme.buildScheme . GHC.unLoc $ Import.importDecl imp)
    endDoc =
      GHC.hang
        (GHC.cat
           [ GHC.text "But it may only be imported in the followings ways"
           , GHC.colon
           ]
        )
        4
        (GHC.vcat $ fmap rebuildFromImport alloweds)
  in
    GHC.vcat
      [ beginningDoc
      , GHC.blankLine
      , endDoc
      , GHC.blankLine
      ]

-- | Rebuild the part of an import statement we subject to rules, namely everything up to explicit
-- items imported or hiding
rebuildImportStatementFromScheme :: GHC.ModuleName -> Scheme.Scheme -> GHC.SDoc
rebuildImportStatementFromScheme modName schema =
  GHC.hsep
  [ GHC.dot
  , GHC.hsep
    [ GHC.text "import"
    , case Scheme.safe schema of
          Scheme.WithoutSafe -> GHC.empty -- Purposely do not mention the safe keyword when not forced, as
                                          -- this makes the output needlessly complex and we don't need to
                                          -- mention something requiring an extension if users didn't ask for
                                          -- it explicitly.
          Scheme.WithSafe -> GHC.text "safe"
    , case Scheme.qualification schema of
          GHC.QualifiedPre -> GHC.text "qualified"
          GHC.QualifiedPost -> GHC.empty
          GHC.NotQualified -> GHC.empty
    , case Scheme.package schema of
          Scheme.WithoutPackage -> GHC.empty -- Purposely do not mention package imports when not forced, as this
                                      -- makes the output needlessly complex and we don't need to
                                      -- mention something requiring an extension if users didn't ask
                                      -- for it explicitly.
          Scheme.WithPackage pkg -> GHC.doubleQuotes $ GHC.text pkg
    , GHC.text (GHC.moduleNameString modName)
    , case Scheme.qualification schema of
          GHC.QualifiedPre -> GHC.empty
          GHC.QualifiedPost -> GHC.text "qualifed"
          GHC.NotQualified -> GHC.empty
    , case Scheme.alias schema of
          Scheme.WithoutAlias ->
            GHC.empty
          Scheme.WithAlias name ->
            GHC.hsep
              [ GHC.text "as"
              , GHC.text (GHC.moduleNameString name)
              ]
    ]
  ]
