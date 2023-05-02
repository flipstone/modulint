{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Henforcer.HaddockCheck.DocumentationAmountRule
  ( DocumentationAmountRule (..)
  , DocumentationAmountViolation (..)
  , documentationRuleDecoder
  , BothDocumentationAmountRule (..)
  ) where

import qualified Data.Text as T
import qualified Dhall
import qualified Numeric.Natural as Nat

import qualified CompatGHC

data DocumentationAmountRule
  = MaximumUndocumented MaxUndocumentedNat
  | -- | The maximum documentable items that are allowed to be undocumented. Note this includes the module header.
    MinimumDocumented MinDocumentedNat
  | -- | Minimum documentable items that must be documented. Note this includes the module header.
    Both BothDocumentationAmountRule

documentationRuleDecoder :: Dhall.Decoder DocumentationAmountRule
documentationRuleDecoder =
  Dhall.union $
    ( MaximumUndocumented
        <$> Dhall.constructor (T.pack "MaximumUndocumented") (fmap MaxUndocumentedNat Dhall.natural)
    )
      <> ( MinimumDocumented
            <$> Dhall.constructor (T.pack "MinimumDocumented") (fmap MinDocumentedNat Dhall.natural)
         )
      <> (fmap Both $ Dhall.constructor (T.pack "Both") bothDocumentationAmountRuleDecoder)

data BothDocumentationAmountRule = BothDocumentationAmountRule
  { maximumUndocumented :: MaxUndocumentedNat
  , minimumDocumented :: MinDocumentedNat
  }

bothDocumentationAmountRuleDecoder :: Dhall.Decoder BothDocumentationAmountRule
bothDocumentationAmountRuleDecoder =
  Dhall.record $
    BothDocumentationAmountRule
      <$> Dhall.field (T.pack "maximumUndocumented") (fmap MaxUndocumentedNat Dhall.natural)
      <*> Dhall.field (T.pack "minimumDocumented") (fmap MinDocumentedNat Dhall.natural)

-- | A wrapper around 'Nat.Natural' to differentiate between various number usages.
newtype MaxUndocumentedNat = MaxUndocumentedNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral)

-- | A wrapper around 'Nat.Natural' to differentiate between various number usages.
newtype MinDocumentedNat = MinDocumentedNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral)

data DocumentationAmountViolation
  = UnderMinimumDocumented MinDocumentedNat Int CompatGHC.ModuleName
  | OverMaximumUndocumented MaxUndocumentedNat Int CompatGHC.ModuleName
  | UnderAndOver MinDocumentedNat MaxUndocumentedNat Int CompatGHC.ModuleName

instance CompatGHC.Outputable DocumentationAmountViolation where
  ppr dv =
    case dv of
      UnderMinimumDocumented minNumDoc actualNumDoc name -> formatUnderMinimumDocumented minNumDoc actualNumDoc name
      OverMaximumUndocumented maxNumUndoc actualNumDoc name -> formatOverMaximumUndocumented maxNumUndoc actualNumDoc name
      UnderAndOver minNumDoc maxNumUndoc actualNumDoc name -> formatOverAndUnder minNumDoc maxNumUndoc actualNumDoc name

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic DocumentationAmountViolation where
  diagnosticMessage = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const []

formatUnderMinimumDocumented :: MinDocumentedNat -> Int -> CompatGHC.ModuleName -> CompatGHC.SDoc
formatUnderMinimumDocumented (MinDocumentedNat nat) actualDocumented name =
  CompatGHC.sep
    [ CompatGHC.hsep
        [ CompatGHC.text "The module"
        , CompatGHC.text (CompatGHC.moduleNameString name)
        , CompatGHC.text "does not meet the minimum number of haddockable items."
        , CompatGHC.text "It must have at least"
        , CompatGHC.text (show nat)
        , CompatGHC.text "but the documented items, including the module itself, was only"
        , CompatGHC.text (show actualDocumented)
        ]
    , CompatGHC.blankLine
    ]

formatOverMaximumUndocumented :: MaxUndocumentedNat -> Int -> CompatGHC.ModuleName -> CompatGHC.SDoc
formatOverMaximumUndocumented (MaxUndocumentedNat nat) actualUndocumented name =
  CompatGHC.sep
    [ CompatGHC.hsep
        [ CompatGHC.text "The module"
        , CompatGHC.text (CompatGHC.moduleNameString name)
        , CompatGHC.text "has more than the maximum number of undocumented items."
        , CompatGHC.text "It must have at most"
        , CompatGHC.text (show nat)
        , CompatGHC.text "but the undocumented items was"
        , CompatGHC.text (show actualUndocumented)
        ]
    , CompatGHC.blankLine
    ]

formatOverAndUnder ::
  MinDocumentedNat
  -> MaxUndocumentedNat
  -> Int
  -> CompatGHC.ModuleName
  -> CompatGHC.SDoc
formatOverAndUnder minDoc maxUndoc actualDocumented name =
  CompatGHC.sep
    [ formatUnderMinimumDocumented minDoc actualDocumented name
    , formatOverMaximumUndocumented maxUndoc actualDocumented name
    ]
