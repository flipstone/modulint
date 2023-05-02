module Henforcer.HaddockCheck.ModuleHeaderRule
  ( ModuleHeaderRule (..)
  , moduleHeaderRuleDecoder
  , ModuleHeaderViolation (..)
  ) where

import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC

data ModuleHeaderViolation
  = MissingDescription CompatGHC.ModuleName
  | MissingCopyright CompatGHC.ModuleName
  | MissingLicense CompatGHC.ModuleName
  | MissingMaintainer CompatGHC.ModuleName
  | MissingStability CompatGHC.ModuleName
  | MissingPortability CompatGHC.ModuleName
  | MissingSafety CompatGHC.ModuleName
  | MissingLanguage CompatGHC.ModuleName
  | MissingExtensions CompatGHC.ModuleName

data ModuleHeaderRule = ModuleHeaderRule
  { mustContainDescription :: Bool
  , mustContainCopyright :: Bool
  , mustContainLicense :: Bool
  , mustContainMaintainer :: Bool
  , mustContainStability :: Bool
  , mustContainPortablity :: Bool
  , mustContainSafety :: Bool
  , mustContainLanguage :: Bool
  , mustContainExtensions :: Bool
  }

moduleHeaderRuleDecoder :: Dhall.Decoder ModuleHeaderRule
moduleHeaderRuleDecoder =
  Dhall.record $
    ModuleHeaderRule
      <$> Dhall.field (T.pack "mustContainDescription") Dhall.bool
      <*> Dhall.field (T.pack "mustContainCopyright") Dhall.bool
      <*> Dhall.field (T.pack "mustContainLicense") Dhall.bool
      <*> Dhall.field (T.pack "mustContainMaintainer") Dhall.bool
      <*> Dhall.field (T.pack "mustContainStability") Dhall.bool
      <*> Dhall.field (T.pack "mustContainPortablity") Dhall.bool
      <*> Dhall.field (T.pack "mustContainSafety") Dhall.bool
      <*> Dhall.field (T.pack "mustContainLanguage") Dhall.bool
      <*> Dhall.field (T.pack "mustContainExtensions") Dhall.bool

instance CompatGHC.Outputable ModuleHeaderViolation where
  ppr mhv =
    let (componentMissingString, modName) =
          case mhv of
            MissingDescription name -> ("description", name)
            MissingCopyright name -> ("copyright", name)
            MissingLicense name -> ("license", name)
            MissingMaintainer name -> ("maintainer", name)
            MissingStability name -> ("stability", name)
            MissingPortability name -> ("portability", name)
            MissingSafety name -> ("safety", name)
            MissingLanguage name -> ("language", name)
            MissingExtensions name -> ("extensions", name)
     in CompatGHC.sep
          [ CompatGHC.hsep
              [ CompatGHC.text "The module"
              , CompatGHC.text (CompatGHC.moduleNameString modName)
              , CompatGHC.text "is missing the"
              , CompatGHC.text componentMissingString
              , CompatGHC.text "field of the haddock module header"
              ]
          ]

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic ModuleHeaderViolation where
  diagnosticMessage = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const []
