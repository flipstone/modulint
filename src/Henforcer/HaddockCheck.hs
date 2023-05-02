module Henforcer.HaddockCheck
  ( checkOnlyGivenModule
  , documentationRulesDecoder
  , DocumentationRules (..)
  , documentationRuleAppliesToModule
  ) where

import Henforcer.HaddockCheck.DocumentationEnforcements (checkOnlyGivenModule)
import Henforcer.HaddockCheck.DocumentationRule
  ( DocumentationRules (..)
  , documentationRuleAppliesToModule
  , documentationRulesDecoder
  )
