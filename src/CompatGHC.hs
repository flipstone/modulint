{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CompatGHC
  ( -- GHC
    GhcRn
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LImportDecl
  , ModSummary (..)
  , ModuleName
  , SrcSpan
  , getLoc
  , ideclAs
  , ideclName
  , ideclQualified
  , ideclSafe
  , ideclHiding
  , locA
  , ml_hs_file
  , moduleName
  , moduleNameString
  , unLoc
  -- GHC.Plugins
  , CommandLineOption
  , DiagnosticReason (..)
  , Messages
  , Outputable (ppr)
  , Plugin (..)
  , SDoc
  , TcGblEnv(tcg_rn_imports, tcg_mod)
  , TcM
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
  , addMessages
  , mkMessagesFromList
  , mkErrorMsgEnvelope
  , mkErrorMsgsWithGeneratedSrcSpan
  , moduleNameDecoder
  ) where

import qualified Dhall
import GHC
  ( GhcRn
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LImportDecl
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
  , ml_hs_file
  , moduleName
  , moduleNameString
  , unLoc
  )
import qualified GHC.Data.Bag as GHC
import GHC.Plugins
  ( CommandLineOption
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
import GHC.Tc.Utils.Monad (TcGblEnv(tcg_rn_imports, tcg_mod), TcM)
import qualified GHC.Tc.Utils.Monad as GHC
import qualified GHC.Types.Error as GHC
import GHC.Types.SourceText (StringLiteral (sl_fs))

#if __GLASGOW_HASKELL__ == 902
import GHC.Types.Error(Messages, MsgEnvelope(..))

#else

import Data.Typeable (Typeable)
import GHC.Plugins (DiagnosticReason(..), Messages)
import qualified GHC.Tc.Errors.Types as GHC904
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

-- | 'mkSimpleDecorated' compat as needed for 9.2.x
mkSimpleDecorated :: SDoc -> GHC.DecoratedSDoc
mkSimpleDecorated = GHC.mkDecorated . pure

-- | Helper for creating a 'MsgEnvelope' as needed for 9.2.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan =
  GHC.mkErr msgSpan GHC.neverQualify

-- | Helper to add messages to the type checking monad so our plugin will print our output and fail
-- a build.
addMessages :: (Diagnostic a) => Messages a -> TcM ()
addMessages =
  GHC.addMessages . fmap diagnosticMessage

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

-- | Helper to add messages to the type checking monad so our plugin will print our output and fail
-- a build.
addMessages :: (Typeable a, Diagnostic a) => Messages a -> TcM ()
addMessages =
  GHC.addMessages . fmap GHC904.TcRnUnknownMessage

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
