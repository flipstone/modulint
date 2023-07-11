{- |
Module      : Henforcer.HaddockCheck.DocumentationRule
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
-}
module Henforcer.HaddockCheck.DocumentationRule
  ( DocumentationRules (..)
  , documentationRulesDecoder
  , documentationRuleAppliesToModule
  , DocumentationViolation (..)
  ) where

import qualified Data.Map.Strict as M
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

{- | Union between the various sorts of violations of documentation rules.
-}
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

-- | Collection of rules pertaining to documentation.
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
      <*> Dhall.field (T.pack "perModuleDocumentationAmounts") (Dhall.map CompatGHC.moduleNameDecoder documentationRuleDecoder)
      <*> Dhall.field (T.pack "moduleHeaderRules") (Dhall.map CompatGHC.moduleNameDecoder moduleHeaderRuleDecoder)

-- | Determines if the documentation rules apply to a given module
documentationRuleAppliesToModule :: DocumentationRules -> CompatGHC.ModuleName -> Bool
documentationRuleAppliesToModule rules modName =
  globalDocumentationAmountRuleApplies (globalDocumentationAmount rules)
    || ( M.member modName (perModuleDocumentationAmounts rules)
          || M.member modName (moduleHeaderRules rules)
       )

-- | The global documentation rule should apply only if it is set and it isn't exactly set to say a
-- minimum of 0, which would be pointless to check anyway.
globalDocumentationAmountRuleApplies :: Maybe DocumentationAmountRule -> Bool
globalDocumentationAmountRuleApplies Nothing = False
globalDocumentationAmountRuleApplies (Just (MinimumDocumented x)) = x /= 0
globalDocumentationAmountRuleApplies _ = True
