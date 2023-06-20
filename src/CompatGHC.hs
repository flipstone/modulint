{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CompatGHC
  ( -- GHC
    GhcRn
  , GhcPs
  , HsModule (..)
  , HsParsedModule (..)
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LImportDecl
  , ModLocation (..)
  , ModSummary (..)
  , ModuleName
  , RawPkgQual (..)
  , SrcSpan
  , getLoc
  , ideclAs
  , ideclName
  , ideclPkgQual
  , ideclQualified
  , ideclSafe
  , ideclHiding
  , locA
  , mkModuleName
  , moduleName
  , moduleNameString
  , unLoc
  -- GHC.Plugins
  , CommandLineOption
  , DiagnosticReason (..)
  , Hsc
  , Messages
  , Outputable (ppr)
  , ParsedResult
  , parsedResultModule
  , Plugin (..)
  , SDoc
  , UnitId (..)
  , blankLine
  , cat
  , colon
  , defaultPlugin
  , dot
  , doubleQuotes
  , empty
  , generatedSrcSpan
  , hang
  , hcat
  , hsep
  , liftIO
  , purePlugin
  , sep
  , text
  , unpackFS
  , vcat
  -- GHC.Types.Error
  , Diagnostic (..)
  , MsgEnvelope
  , mkSimpleDecorated
  -- GHC.Types.SourceText
  , sl_fs
  -- internal defined helpers
  , mkMessagesFromList
  , printMsgs
  , mkErrorMsgEnvelope
  , mkErrorMsgsWithGeneratedSrcSpan
  , moduleNameDecoder
  ) where

import qualified Dhall
import GHC
  ( GhcPs
  , GhcRn
  , HsModule (..)
  , HsParsedModule (..)
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LImportDecl
  , ModLocation (..)
  , ModSummary (..)
  , ModuleName
  , SrcSpan
  , getLoc
  , ideclAs
  , ideclHiding
  , ideclName
  , ideclQualified
  , ideclSafe
  , locA
  , mkModuleName
  , moduleName
  , moduleNameString
  , unLoc
  )
import qualified GHC
import qualified GHC.Data.Bag as GHC
import GHC.Plugins
  ( CommandLineOption
  , Hsc
  , Outputable (ppr)
  , Plugin (..)
  , SDoc
  , UnitId (..)
  , blankLine
  , cat
  , colon
  , defaultPlugin
  , dot
  , doubleQuotes
  , empty
  , generatedSrcSpan
  , hang
  , hcat
  , hsep
  , liftIO
  , purePlugin
  , sep
  , text
  , unpackFS
  , vcat
  )
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Error as GHC
import GHC.Types.SourceText (StringLiteral (sl_fs))

#if __GLASGOW_HASKELL__ == 902
import qualified GHC.Driver.Errors as GHC902
import GHC.Types.Error(Messages, MsgEnvelope(..))

#else

import GHC (ideclPkgQual)
import qualified GHC.Driver.Config.Diagnostic as GHC904
import qualified GHC.Driver.Errors as GHC904
import GHC.Plugins (DiagnosticReason(..), Messages, ParsedResult(..), RawPkgQual(..))
import GHC.Types.Error (MsgEnvelope(..), Diagnostic(..), mkSimpleDecorated)

#endif

#if __GLASGOW_HASKELL__ == 902

-- | The bare minimum of the GHC 9.4 'DiagnosticReason' type that we need to support in 9.2
data DiagnosticReason = ErrorWithoutFlag

-- While we never create any values of this type, it allows us to keep the 'Diagnostic' interface
-- the same as in 9.4
data GhcHint

-- | The bare minimum of the GHC 9.4 'Diagnostic' class as needed to support 9.2
class Diagnostic a where
  diagnosticMessage :: a -> GHC.DecoratedSDoc
  diagnosticReason  :: a -> DiagnosticReason
  diagnosticHints   :: a -> [GhcHint]

instance {-# INCOHERENT #-} (Diagnostic a) => GHC.RenderableDiagnostic a where
  renderDiagnostic = diagnosticMessage

data RawPkgQual
  = NoRawPkgQual
  | RawPkgQual StringLiteral

-- | In GHC 9.4 a new result type was added, but 9.2 used the 'HsParsedModule'
type ParsedResult = HsParsedModule

parsedResultModule :: ParsedResult -> ParsedResult
parsedResultModule = id

-- | Internal helper for GHC 9.2 message printing
printMessages :: Diagnostic a => GHC.Logger -> GHC.DynFlags -> Messages a -> IO ()
printMessages l f msgs =
  let (warns, errs) = GHC.partitionMessages msgs
      msgsToPrint = GHC.unionBags warns errs
  in
    GHC902.printBagOfErrors l f msgsToPrint

-- | 'mkSimpleDecorated' compat as needed for 9.2.x
mkSimpleDecorated :: SDoc -> GHC.DecoratedSDoc
mkSimpleDecorated = GHC.mkDecorated . pure

-- | Wrap the 9.2.x 'ideclPkgQual' so to be compatible with 9.4.x
ideclPkgQual :: ImportDecl pass -> RawPkgQual
ideclPkgQual =
  maybe NoRawPkgQual (RawPkgQual) . GHC.ideclPkgQual

-- | Helper for creating a 'MsgEnvelope' as needed for 9.2.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan =
  GHC.mkErr msgSpan GHC.neverQualify

-- | Helper for printing messages as needed for GHC 9.2.x
printMsgs :: Diagnostic a => Messages a -> Hsc ()
printMsgs msgs = do
  ghcLogger <- GHC.getLogger
  diagOpts <- GHC.getDynFlags
  liftIO $ printMessages ghcLogger diagOpts msgs

#endif

#if __GLASGOW_HASKELL__ == 904

-- | Helper for creating a 'MsgEnvelope' as needed for 9.4.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan a =
  MsgEnvelope
    { errMsgSpan = msgSpan
    , errMsgContext = GHC.neverQualify
    , errMsgSeverity = GHC.SevError
    , errMsgDiagnostic = a
    }

-- | Helper for printing messages as needed for GHC 9.4.x
printMsgs :: Diagnostic a => Messages a -> Hsc ()
printMsgs msgs = do
  ghcLogger <- GHC.getLogger
  diagOpts <- fmap GHC904.initDiagOpts GHC.getDynFlags
  liftIO $ GHC904.printMessages ghcLogger diagOpts msgs

#endif

-- | Helper for creating 'Messages'
mkMessagesFromList :: [MsgEnvelope e] -> Messages e
mkMessagesFromList = GHC.mkMessages . GHC.listToBag

-- | Helper for creating messages with a fake src span, useful for when we do not otherwise have one, such as in haddock checks.
mkErrorMsgsWithGeneratedSrcSpan :: [e] -> Messages e
mkErrorMsgsWithGeneratedSrcSpan =
  mkMessagesFromList . fmap (mkErrorMsgEnvelope generatedSrcSpan)

-- | Helper function to abstract decoding ModuleName to a central place
moduleNameDecoder :: Dhall.Decoder ModuleName
moduleNameDecoder =
  fmap mkModuleName Dhall.string
