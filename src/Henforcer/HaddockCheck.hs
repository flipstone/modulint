{- |
Module      : Henforcer.Haddock.Check
Description : All of the functionality around Haddock rule definition and enforcement.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
-}
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
