{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Henforcer.HaddockCheck.DocumentationRule
  ( DocumentationRules (..)
  , documentationRulesDecoder
  , documentationRuleAppliesToModule
  , DocumentationViolation (..)
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC
import Henforcer.HaddockCheck.DocumentationAmountRule
  ( DocumentationAmountRule (..)
  , DocumentationAmountViolation (..)
  , documentationRuleDecoder
  )
import Henforcer.HaddockCheck.ModuleHeaderRule
  ( ModuleHeaderRule (..)
  , ModuleHeaderViolation (..)
  , moduleHeaderRuleDecoder
  )

data DocumentationViolation
  = AmountViolation DocumentationAmountViolation
  | HeaderViolation ModuleHeaderViolation

instance CompatGHC.Outputable DocumentationViolation where
  ppr dv =
    case dv of
      AmountViolation dav -> CompatGHC.ppr dav
      HeaderViolation hv -> CompatGHC.ppr hv

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic DocumentationViolation where
  diagnosticMessage = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const []

data DocumentationRules = DocumentationRules
  { globalDocumentationAmount :: Maybe DocumentationAmountRule
  , perModuleDocumentationAmounts :: M.Map CompatGHC.ModuleName DocumentationAmountRule
  , moduleHeaderRules :: M.Map CompatGHC.ModuleName ModuleHeaderRule
  }

documentationRulesDecoder :: Dhall.Decoder DocumentationRules
documentationRulesDecoder =
  Dhall.record $
    DocumentationRules
      <$> Dhall.field (T.pack "globalDocumentationAmount") (Dhall.maybe documentationRuleDecoder)
      <*> Dhall.field (T.pack "perModuleDocumentationAmounts") (Dhall.map moduleName documentationRuleDecoder)
      <*> Dhall.field (T.pack "moduleHeaderRules") (Dhall.map moduleName moduleHeaderRuleDecoder)

documentationRuleAppliesToModule :: DocumentationRules -> CompatGHC.ModuleName -> Bool
documentationRuleAppliesToModule rules modName =
  if Maybe.isJust (globalDocumentationAmount rules)
    then True
    else
      M.member modName (perModuleDocumentationAmounts rules)
        || M.member modName (moduleHeaderRules rules)

-- TODO combine this with other definition
moduleName :: Dhall.Decoder CompatGHC.ModuleName
moduleName =
  fmap CompatGHC.mkModuleName Dhall.string
