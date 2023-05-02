module Henforcer.HaddockCheck.DocumentationEnforcements
  ( checkInterfaces
  , checkOnlyGivenModule
  , checkInterfaceAgainstHeaderRuleForViolations
  ) where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Documentation.Haddock as Haddock

import qualified CompatGHC
import Henforcer.HaddockCheck.DocumentationAmountRule
  ( BothDocumentationAmountRule (..)
  , DocumentationAmountRule (..)
  , DocumentationAmountViolation (..)
  )
import Henforcer.HaddockCheck.DocumentationRule
  ( DocumentationRules (..)
  , DocumentationViolation (..)
  )
import Henforcer.HaddockCheck.ModuleHeaderRule
  ( ModuleHeaderRule (..)
  , ModuleHeaderViolation (..)
  )

{- | Check some documentation rules for interfaces, collecting violations in messages suitable for
 plugin printing.
-}
checkOnlyGivenModule ::
  DocumentationRules -> CompatGHC.ModuleName -> [Haddock.Interface] -> [DocumentationViolation]
checkOnlyGivenModule enforcements name interfaces =
  checkInterfaces enforcements $
    filter
      ( \iface ->
          name == CompatGHC.moduleName (Haddock.ifaceMod iface)
      )
      interfaces

{- | Check some documentation rules for interfaces, collecting violations in messages suitable for
 plugin printing.
-}
checkInterfaces :: DocumentationRules -> [Haddock.Interface] -> [DocumentationViolation]
checkInterfaces enforcements =
  Monad.join . fmap (checkDocumentationRules enforcements)

checkDocumentationRules :: DocumentationRules -> Haddock.Interface -> [DocumentationViolation]
checkDocumentationRules rulesToEnforce iface =
  let name = CompatGHC.moduleName $ Haddock.ifaceMod iface
      amountViolations =
        fmap AmountViolation . Maybe.maybeToList $
          case M.lookup name (perModuleDocumentationAmounts rulesToEnforce) of
            Just rule -> checkInterfaceAgainstRuleForViolation iface rule
            Nothing ->
              Monad.join . fmap (checkInterfaceAgainstRuleForViolation iface) $
                globalDocumentationAmount rulesToEnforce
      headerViolations =
        case M.lookup name (moduleHeaderRules rulesToEnforce) of
          Nothing -> []
          Just rule ->
            fmap HeaderViolation $ checkInterfaceAgainstHeaderRuleForViolations iface rule
   in amountViolations <> headerViolations

checkInterfaceAgainstRuleForViolation ::
  Haddock.Interface -> DocumentationAmountRule -> Maybe DocumentationAmountViolation
checkInterfaceAgainstRuleForViolation interface rule =
  let name = CompatGHC.moduleName $ Haddock.ifaceMod interface
      (documentable, documented) = Haddock.ifaceHaddockCoverage interface
   in case rule of
        (MaximumUndocumented maxNumUndoc) ->
          let undocumented = (documentable - documented)
           in if undocumented > fromIntegral maxNumUndoc
                then Just $ OverMaximumUndocumented maxNumUndoc undocumented name
                else Nothing
        (MinimumDocumented minNumDoc) ->
          if documented < fromIntegral minNumDoc
            then Just $ UnderMinimumDocumented minNumDoc documented name
            else Nothing
        (Both (BothDocumentationAmountRule maxNumUndoc minNumDoc)) ->
          let undocumented = (documentable - documented)
           in if documented < fromIntegral minNumDoc
                then
                  if undocumented > fromIntegral maxNumUndoc
                    then Just $ UnderAndOver minNumDoc maxNumUndoc undocumented name
                    else Just $ UnderMinimumDocumented minNumDoc documented name
                else
                  if undocumented > fromIntegral maxNumUndoc
                    then Just $ OverMaximumUndocumented maxNumUndoc undocumented name
                    else Nothing

checkInterfaceAgainstHeaderRuleForViolations ::
  Haddock.Interface -> ModuleHeaderRule -> [ModuleHeaderViolation]
checkInterfaceAgainstHeaderRuleForViolations interface rule =
  let info = Haddock.ifaceInfo interface
      name = CompatGHC.moduleName $ Haddock.ifaceMod interface
      rules =
        Maybe.catMaybes
          [ if mustContainDescription rule
              then Just checkForDescription
              else Nothing
          , if mustContainCopyright rule
              then Just checkForCopyright
              else Nothing
          , if mustContainLicense rule
              then Just checkForLicense
              else Nothing
          , if mustContainMaintainer rule
              then Just checkForMaintainer
              else Nothing
          , if mustContainStability rule
              then Just checkForStability
              else Nothing
          , if mustContainPortablity rule
              then Just checkForPortability
              else Nothing
          , if mustContainSafety rule
              then Just checkForSafety
              else Nothing
          , if mustContainLanguage rule
              then Just checkForLanguage
              else Nothing
          , if mustContainExtensions rule
              then Just checkForExtensions
              else Nothing
          ]
   in Maybe.catMaybes $ fflap rules info name

checkForDescription ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForDescription interfaceInfo modName =
  case Haddock.hmi_description interfaceInfo of
    Nothing -> Just $ MissingDescription modName
    Just Haddock.DocEmpty -> Just $ MissingDescription modName
    Just _ -> Nothing

checkForCopyright ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForCopyright interfaceInfo modName =
  case Haddock.hmi_copyright interfaceInfo of
    Nothing -> Just $ MissingCopyright modName
    Just _ -> Nothing

checkForLicense ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForLicense interfaceInfo modName =
  case Haddock.hmi_license interfaceInfo of
    Nothing -> Just $ MissingLicense modName
    Just _ -> Nothing

checkForMaintainer ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForMaintainer interfaceInfo modName =
  case Haddock.hmi_maintainer interfaceInfo of
    Nothing -> Just $ MissingMaintainer modName
    Just _ -> Nothing

checkForStability ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForStability interfaceInfo modName =
  case Haddock.hmi_stability interfaceInfo of
    Nothing -> Just $ MissingStability modName
    Just _ -> Nothing

checkForPortability ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForPortability interfaceInfo modName =
  case Haddock.hmi_portability interfaceInfo of
    Nothing -> Just $ MissingPortability modName
    Just _ -> Nothing

checkForSafety :: Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForSafety interfaceInfo modName =
  case Haddock.hmi_safety interfaceInfo of
    Nothing -> Just $ MissingSafety modName
    Just _ -> Nothing

checkForLanguage ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForLanguage interfaceInfo modName =
  case Haddock.hmi_language interfaceInfo of
    Nothing -> Just $ MissingLanguage modName
    Just _ -> Nothing

checkForExtensions ::
  Haddock.HaddockModInfo name -> CompatGHC.ModuleName -> Maybe ModuleHeaderViolation
checkForExtensions interfaceInfo modName =
  case Haddock.hmi_extensions interfaceInfo of
    [] -> Just $ MissingExtensions modName
    _ -> Nothing

flap :: (Functor f) => f (a -> b) -> a -> f b
flap functorFn val =
  fmap (\fn -> fn val) functorFn

fflap :: (Functor f) => f (a -> b -> c) -> a -> b -> f c
fflap functorFn x y =
  flip flap y $ flap functorFn x
