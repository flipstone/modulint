{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module CompatGHC
  ( HsModule(..)
  , HsParsedModule
  , ParsedResult(..)
  , Messages
  , MsgEnvelope
  , unLoc
  , RawPkgQual(..)
  -- Doc and printing related stuff
  , SDoc
  , text
  , hsep
  , sep
  , empty
  , doubleQuotes
  , dot
  , blankLine
  , colon
  , cat
  , hcat
  , vcat
  , hang
  , Outputable(ppr)
  , SrcSpan
  , DiagnosticReason(..)
  , Diagnostic(..)

  , mkSimpleDecorated

  , Plugin(..)
  , defaultPlugin
  , purePlugin
  , liftIO
  , CommandLineOption
  , getLoc

  , Hsc

  , ModuleName
  , moduleNameString
  , mkModuleName

  -- GHC.Data.FastString
  , unpackFS

  -- GHC.Driver.Session
  , DynFlags
  , initDynFlags
  , defaultDynFlags
  , GeneralFlag(Opt_Haddock,Opt_KeepRawTokenStream)

  -- GHC
  , locA

  -- GHC.Hs.Extension
  , GhcPs

  -- GHC.Hs.ImpExp
  , ImportDecl
  , ideclPkgQual
  , ideclAs
  , ideclSafe
  , ideclQualified
  , ImportDeclQualifiedStyle(..)
  , LImportDecl
  , ideclName

  -- GHC.Types.SourceText
  , sl_fs

  -- internal defined helpers
  , PluginResult
  , renderWithDefaultContext
  , mkMessagesFromList
  , getHsModule
  , printMsgs
  , mkErrorMsgEnvelope
  , fakeLlvmConfig
  , fakeSettings
  , runParser
  )
  where

import qualified GHC
import GHC ( GeneralFlag(Opt_Haddock,Opt_KeepRawTokenStream)
           , GhcPs
           , HsModule(..)
           , ImportDecl
           , LImportDecl
           , DynFlags
           , ImportDeclQualifiedStyle(..)
           , ideclAs
           , ideclSafe
           , ideclQualified
           , ideclName
           )
import qualified GHC.Data.Bag as GHC
import GHC.Data.FastString (unpackFS)
import qualified GHC.Data.StringBuffer as GHC
import GHC.Parser (parseHeader)
import qualified GHC.Parser.Lexer as GHC
import GHC.Plugins (
    SDoc
  , text
  , hsep
  , sep
  , empty
  , doubleQuotes
  , dot
  , blankLine
  , colon
  , cat
  , hcat
  , vcat
  , hang
  , Outputable(ppr)
  , SrcSpan
  , Hsc, liftIO, unLoc, getDynFlags, Plugin(..), defaultPlugin, purePlugin
  , neverQualify
  , ModuleName
  , moduleNameString
  , mkModuleName
  , CommandLineOption
  , getLoc
  , LlvmConfig(LlvmConfig)
  , initDynFlags
  , defaultDynFlags
  )
import qualified GHC.Plugins as GHC

-- For building a Settings and LlvmConfig. This is easily the most frustrating part of working with the ghc api
import qualified GHC.Utils.Fingerprint as GHC
import qualified GHC.Settings as GHC

#if __GLASGOW_HASKELL__ == 900
import qualified GHC.ByteOrder as GHC900
import qualified GHC.Platform as GHC900
import GHC.Types.Basic(StringLiteral(sl_fs))
import GHC.Plugins( HsParsedModule(..))
import qualified GHC.Utils.Error as GHC900

#elif __GLASGOW_HASKELL__ == 902
import qualified Control.Monad as M
import GHC (locA,  HsParsedModule(..))
import qualified GHC.Types.Error as GHC902
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Driver.Errors as GHC902
import GHC.Types.Error(DecoratedSDoc, MsgEnvelope(..), Messages)
import qualified GHC.Parser.Errors.Ppr as GHC902
import GHC.Types.SourceText (StringLiteral(sl_fs))
import GHC.Parser.Lexer (initParserState)
import GHC.Platform (genericPlatform)

#else

import GHC (extensionFlags, HsParsedModule(..), locA, ideclPkgQual)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Errors (printMessages)
import GHC.Parser.Lexer (getPsErrorMessages, initParserState)
import GHC.Platform (genericPlatform)
import GHC.Plugins (ParsedResult(..), DiagnosticReason(..), RawPkgQual(..))
import GHC.Types.Error (MsgEnvelope(..), Severity(..), mkMessages, Messages, diagnosticMessage, Diagnostic(..), mkSimpleDecorated)
import GHC.Types.SourceText (StringLiteral(sl_fs))

#endif
-- #else
-- import GHC.Plugins (ParsedResult, Messages, parsedResultModule, RawPkgQual(..), DiagnosticReason(..))
-- import GHC.Driver.Errors (printMessages)
-- import GHC.Driver.Config.Diagnostic (initDiagOpts)
-- import GHC.Parser.Lexer (getPsErrorMessages)
-- import GHC.Hs.ImpExp (ImportDecl(..))
-- import GHC.Types.Error( Diagnostic(..), GhcHint(..), mkSimpleDecorated)
-- #endif


#if __GLASGOW_HASKELL__ <= 900

-- | 'locA' compat shim that for ghc9.0.x is the identity.
locA :: a -> a
locA = id

-- | 'DecoratedSDoc' compat, that is simply an alias for '[SDoc]'
type DecoratedSDoc = [SDoc]

-- | The bare minimum of the GHC 9.4 'ParsedResult' type that we need to support in 9.0.x
newtype ParsedResult =
  ParsedResult
    { parsedResultModule :: HsParsedModule
    }

-- | The bare minimum of the GHC 9.2/9.4 'MsgEnvelope' type that we need to support in 9.0.x
data MsgEnvelope e =
  MsgEnvelope
    { errMsgDiagnostic :: e
    , errMsgSpan :: SrcSpan
    }

-- | The bare minimum of the GHC 9.2/9.4 'Messages' type that we need to support in 9.0
newtype Messages e = Messages (GHC.Bag (MsgEnvelope e))

-- | The bare minimum of the GHC 9.4 'ParsedResult' type that we need to support in 9.0
data RawPkgQual
  = NoRawPkgQual
  | RawPkgQual StringLiteral

-- | The bare minimum of the GHC 9.4 'DiagnosticReason' type that we need to support in 9.0
data DiagnosticReason = ErrorWithoutFlag

-- While we never create any values of this type, it allows us to keep the 'Diagnostic' interface
-- the same as in 9.4
data GhcHint

-- | The bare minimum of the GHC 9.4 'Diagnostic' class as needed to support 9.0
class Diagnostic a where
  diagnosticMessage :: a -> DecoratedSDoc
  diagnosticReason  :: a -> DiagnosticReason
  diagnosticHints   :: a -> [GhcHint]

-- | 'ParserOpts' compat, that is simply an alias for 'ParserFlags'
type ParserOpts = GHC.ParserFlags

-- | 'initParserState' compat with GHC 9.4.x
initParserState :: ParserOpts
                -> GHC.StringBuffer
                -> GHC.RealSrcLoc
                -> GHC.PState
initParserState = GHC.mkPStatePure

-- | An internal helper building up 'DynFlags' as needed for GHC 9.0.x
defaultFlags :: DynFlags
defaultFlags = defaultDynFlags fakeSettings fakeLlvmConfig

-- | An internal helper to provide the GHC 9.0.x version of the message printing interface.
envelopeToErrMsg :: Diagnostic a => MsgEnvelope a -> GHC900.ErrMsg
envelopeToErrMsg env =
  GHC900.mkErrMsg defaultFlags (errMsgSpan env) neverQualify . sep . diagnosticMessage $ errMsgDiagnostic env

-- | 'mkSimpleDecorated' compat as needed for 9.0.x
mkSimpleDecorated :: SDoc -> DecoratedSDoc
mkSimpleDecorated = pure

-- | Wrap the 9.0.x 'ideclPkgQual' so to be compatible with 9.4.x
ideclPkgQual :: ImportDecl pass -> RawPkgQual
ideclPkgQual =
  maybe NoRawPkgQual RawPkgQual . GHC.ideclPkgQual

-- | Helper for creating a 'MsgEnvelope' as needed for 9.0.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan e =
  MsgEnvelope e msgSpan

-- | Helper for rendering an error doc to a 'String' as needed for GHC 9.0.x
renderWithDefaultContext :: SDoc -> String
renderWithDefaultContext = GHC.renderWithStyle (GHC.initSDocContext defaultFlags GHC.defaultErrStyle)

-- | Internal helper for rendering 'PState' warning and errors to a 'String' for 9.0.x
getPFailedMessage :: GHC.PState -> String
getPFailedMessage pstate =
  let
    (warnBag, errBag) = GHC.getMessages pstate defaultFlags
  in
  renderWithDefaultContext
  . GHC900.pprMessageBag
  . GHC.listToBag
  $ GHC900.pprErrMsgBagWithLoc errBag <> GHC900.pprErrMsgBagWithLoc warnBag

-- | Internal compat for 'genericPlatform' from 9.2/9.4 as needed for GHC 9.0.x
genericPlatform :: GHC900.Platform
genericPlatform =
  GHC900.Platform{
        GHC.platformByteOrder=GHC900.LittleEndian
      , GHC.platformHasGnuNonexecStack=True
      , GHC.platformHasIdentDirective=False
      , GHC.platformHasSubsectionsViaSymbols=False
      , GHC.platformIsCrossCompiling=False
      , GHC.platformLeadingUnderscore=False
      , GHC.platformTablesNextToCode=False
      , GHC.platformWordSize=GHC900.PW8
      , GHC.platformMini=GHC.PlatformMini { GHC.platformMini_arch=GHC900.ArchUnknown
                                          , GHC.platformMini_os=GHC900.OSUnknown
                                          }
      , GHC.platformUnregisterised=True
      }

-- | Helper for creating 'Messages' as needed for GHC 9.0.x
mkMessagesFromList :: [MsgEnvelope e] -> Messages e
mkMessagesFromList = Messages . GHC.listToBag

-- | Helper for printing messages as needed for GHC 9.0.x
printMsgs :: Diagnostic a => Messages a -> Hsc ()
printMsgs (Messages bag) = do
  diagOpts <- getDynFlags
  liftIO . GHC900.printBagOfErrors diagOpts $ GHC.mapBag envelopeToErrMsg bag

-- | Minimal settings needed to run the parser, as needed for GHC 9.0.x
fakeSettings :: GHC.Settings
fakeSettings =
  GHC.Settings
    (GHC.GhcNameVersion "" "")
    (GHC.FileSettings {})
    genericPlatform
    (GHC.ToolSettings {
      GHC.toolSettings_opt_P_fingerprint=GHC.fingerprint0
      }
    )
    (GHC.PlatformMisc {})
    (GHC.PlatformConstants{ GHC.pc_DYNAMIC_BY_DEFAULT=False
                             , GHC.pc_WORD_SIZE=8
                             }
    )
    []

-- | Helper alias for plugin types as needed for GHC 9.0.x
type PluginResult = HsParsedModule

-- | Helper to get the 'HsModule' as needed for GHC 9.0.x
getHsModule :: PluginResult -> HsModule
getHsModule =
  unLoc . hpm_module

-- 'dynFlagsToParserOpts' is an internal helper to build up a more unified interface.
dynFlagsToParserOpts :: DynFlags -> ParserOpts
dynFlagsToParserOpts = GHC.mkParserFlags

#endif

#if __GLASGOW_HASKELL__ == 902

-- | The bare minimum of the GHC 9.4 'ParsedResult' type that we need to support in 9.2.x
newtype ParsedResult =
  ParsedResult
    { parsedResultModule :: HsParsedModule
    }

-- | The bare minimum of the GHC 9.4 'ParsedResult' type that we need to support in 9.2
data RawPkgQual
  = NoRawPkgQual
  | RawPkgQual StringLiteral

-- | The bare minimum of the GHC 9.4 'DiagnosticReason' type that we need to support in 9.2
data DiagnosticReason = ErrorWithoutFlag

-- While we never create any values of this type, it allows us to keep the 'Diagnostic' interface
-- the same as in 9.4
data GhcHint

-- | The bare minimum of the GHC 9.4 'Diagnostic' class as needed to support 9.2
class Diagnostic a where
  diagnosticMessage :: a -> DecoratedSDoc
  diagnosticReason  :: a -> DiagnosticReason
  diagnosticHints   :: a -> [GhcHint]

instance {-# INCOHERENT #-} (Diagnostic a) => GHC902.RenderableDiagnostic a where
  renderDiagnostic = diagnosticMessage

-- | Internal helper for GHC 9.2 message printing
printMessages :: Diagnostic a => GHC.Logger -> DynFlags -> Messages a -> IO ()
printMessages l f msgs =
  let (warns, errs) = GHC902.partitionMessages msgs
      msgsToPrint = GHC.unionBags warns errs
  in
    GHC902.printBagOfErrors l f msgsToPrint

-- | 'mkSimpleDecorated' compat as needed for 9.2.x
mkSimpleDecorated :: SDoc -> DecoratedSDoc
mkSimpleDecorated = GHC902.mkDecorated . pure

-- | Wrap the 9.2.x 'ideclPkgQual' so to be compatible with 9.4.x
ideclPkgQual :: ImportDecl pass -> RawPkgQual
ideclPkgQual =
  maybe NoRawPkgQual RawPkgQual . GHC.ideclPkgQual

-- | Helper for creating a 'MsgEnvelope' as needed for 9.2.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan =
  GHC902.mkErr msgSpan neverQualify

-- | Internal helper for rendering 'PState' warning and errors to a 'String' for 9.2.x
getPFailedMessage :: GHC.PState -> String
getPFailedMessage pstate =
  let
    (warnBag, errBag) = GHC.getMessages pstate
  in
    renderWithDefaultContext
  . GHC902.pprMessageBag
  . GHC.listToBag
  . M.join
  . GHC.bagToList
  . fmap (GHC902.unDecorated . errMsgDiagnostic)
  $ GHC.unionBags (fmap GHC902.pprWarning warnBag) (fmap GHC902.pprError errBag)

-- | Internal helper for building up 'ParserOpts' as needed for GHC 9.2.x
dynFlagsToParserOpts :: DynFlags -> GHC.ParserOpts
dynFlagsToParserOpts flags =
  GHC.mkParserOpts
  EnumSet.empty
  EnumSet.empty
  (GHC.safeHaskellOn flags)
  (GHC.gopt Opt_Haddock flags)
  (GHC.gopt Opt_KeepRawTokenStream flags)
  True

-- | Helper for printing messages as needed for GHC 9.2.x
printMsgs :: Diagnostic a => Messages a -> Hsc ()
printMsgs msgs = do
  ghcLogger <- GHC.getLogger
  diagOpts <- getDynFlags
  liftIO $ printMessages ghcLogger diagOpts msgs

-- | Minimal settings needed to run the parser, as needed for GHC 9.2.x
fakeSettings :: GHC.Settings
fakeSettings =
  GHC.Settings
    (GHC.GhcNameVersion "" "")
    (GHC.FileSettings {})
    genericPlatform
    (GHC.ToolSettings {
      GHC.toolSettings_opt_P_fingerprint=GHC.fingerprint0
      }
    )
    (GHC.PlatformMisc {})
    []

-- | Helper for creating 'Messages' as needed for GHC 9.2.x
mkMessagesFromList :: [MsgEnvelope e] -> Messages e
mkMessagesFromList = GHC902.mkMessages . GHC.listToBag

-- | Helper for rendering an error doc to a 'String' as needed for GHC 9.2.x
renderWithDefaultContext :: SDoc -> String
renderWithDefaultContext = GHC.renderWithContext GHC.defaultSDocContext

-- | Helper alias for plugin types as needed for GHC 9.2.x
type PluginResult = HsParsedModule

-- | Helper to get the 'HsModule' as needed for GHC 9.2.x
getHsModule :: PluginResult -> HsModule
getHsModule =
  unLoc . hpm_module

#endif

#if __GLASGOW_HASKELL__ == 904

-- | Helper for creating 'Messages' as needed for GHC 9.4.x
mkMessagesFromList :: [MsgEnvelope e] -> Messages e
mkMessagesFromList = mkMessages . GHC.listToBag

-- | Helper for creating a 'MsgEnvelope' as needed for 9.4.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan a =
  MsgEnvelope
    { errMsgSpan = msgSpan
    , errMsgContext = neverQualify
    , errMsgSeverity = SevError
    , errMsgDiagnostic = a
    }

-- | Helper for rendering an error doc to a 'String' as needed for GHC 9.4.x
renderWithDefaultContext :: SDoc -> String
renderWithDefaultContext = GHC.renderWithContext GHC.defaultSDocContext

-- | Internal helper for rendering 'PState' warning and errors to a 'String' for 9.4.x
getPFailedMessage :: GHC.PState -> String
getPFailedMessage =
  undefined . fmap diagnosticMessage . getPsErrorMessages

-- | Internal helper for building up 'ParserOpts' as needed for GHC 9.2.x
dynFlagsToParserOpts :: DynFlags -> GHC.ParserOpts
dynFlagsToParserOpts flags =
  GHC.mkParserOpts
  (GHC.extensionFlags flags)
  (initDiagOpts flags)
  (GHC.supportedLanguagesAndExtensions . GHC.platformArchOS $ GHC.targetPlatform flags)
  (GHC.safeHaskellOn flags)
  (GHC.gopt Opt_Haddock flags)
  (GHC.gopt Opt_KeepRawTokenStream flags)
  True

-- | Helper for printing messages as needed for GHC 9.4.x
printMsgs :: Diagnostic a => Messages a -> Hsc ()
printMsgs msgs = do
  ghcLogger <- GHC.getLogger
  diagOpts <- fmap initDiagOpts getDynFlags
  liftIO $ printMessages ghcLogger diagOpts msgs

-- | Minimal settings needed to run the parser, as needed for GHC 9.4.x
fakeSettings :: GHC.Settings
fakeSettings =
  GHC.Settings
    (GHC.GhcNameVersion "" "")
    (GHC.FileSettings {})
    genericPlatform
    (GHC.ToolSettings {
      GHC.toolSettings_opt_P_fingerprint=GHC.fingerprint0
      }
    )
    (GHC.PlatformMisc {})
    []

-- | Helper alias for plugin types as needed for GHC 9.4.x
type PluginResult = ParsedResult

-- | Helper to get the 'HsModule' as needed for GHC 9.4.x
getHsModule :: PluginResult -> HsModule
getHsModule =
  unLoc . hpm_module . parsedResultModule

#endif

-- | Empty LlvmConfig, that is needed, but not ever actually used.
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

-- | Internal helper that will determine the 'HsModule' or if there is a parser error, return that error as a 'String'
getMod :: GHC.PState -> Either String HsModule
getMod pstate =
  let
    unLocPHeader = fmap unLoc parseHeader
  in
    case GHC.unP unLocPHeader pstate of
      GHC.POk _ v ->
        pure v
      GHC.PFailed failedState ->
        Left (getPFailedMessage failedState)

-- | Run the parser for some flags on a given file, hopefully getting an 'HsModule'.
runParser :: DynFlags -> FilePath -> IO (Either String HsModule)
runParser initializedDynFlags fileName = do
  bufferToParse <- GHC.hGetStringBuffer fileName
  let
    location = GHC.mkRealSrcLoc (GHC.mkFastString fileName) 1 1
  pure (getMod (initParserState (dynFlagsToParserOpts initializedDynFlags) bufferToParse location))
