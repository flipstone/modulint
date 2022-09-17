{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PackageImports #-}
module Scratch where

-- import 1
import Control.Exception
import Control.Monad
import qualified Data.Array as Arr
import Data.IORef
import qualified Data.Map.Lazy as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import GHC
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable (ppr, SDoc, initSDocContext, defaultDumpStyle, renderWithStyle, showSDocUnsafe )
import Modulint.GhcLib.Import
import safe qualified "ghc-lib" GHC

readHIE = do
  nameCacheUpdater <-
    mkNameCacheUpdater
  let
--    innerT = fmap (Maybe.catMaybes . S.toList . S.map filterIEs . identInfo)
    toNodeIds = fmap (fmap (fmap innerT) . nodeIds) . (getAsts . hie_asts . hie_file_result)
--    toIdentInfos = fmap identInfo . toNodeIds
    trans = toNodeIds
    trans' = (join . fmap snd . join . fmap (M.toList . snd) . join . fmap M.toList . fmap snd . M.toList . trans)

  hie <- readHieFile nameCacheUpdater "src/Modulint/Check.hie"

  -- dyns <- runGhc Nothing getSessionDynFlags
  -- let
  --   context = initSDocContext dyns defaultDumpStyle
  print $ trans' hie
  print . fmap showSDocUnsafe . detailsForFile $ hie_file_result hie
  pure $ fmap (fmap showHieType) . Arr.assocs . hie_types $ hie_file_result hie

detailsForFile :: HieFile -> [SDoc]
detailsForFile =
  join . fmap innerIdDetails . getAsts'

innerIdDetails :: HieAST a -> [SDoc]
innerIdDetails =
  join . fmap innerT' . onlyIdDetails

onlyIdDetails :: HieAST a -> [IdentifierDetails a]
onlyIdDetails =
  join . fmap (fmap snd . M.toList) . onlyNodeIds

onlyNodeIds :: HieAST a -> [NodeIdentifiers a]
onlyNodeIds =
  fmap snd . M.toList . nodeIds

nodeIds :: HieAST a -> M.Map NodeOrigin (NodeIdentifiers a)
nodeIds =
  fmap nodeIdentifiers . getSourcedNodeInfo . sourcedNodeInfo

innerT' =
  fmap ppr . S.toList . identInfo

innerT :: IdentifierDetails a -> [String]
innerT =
  Maybe.catMaybes . S.toList . S.map filterIEs . identInfo

getAsts' :: HieFile -> [HieAST TypeIndex]
getAsts' =
  flattenASTs . fmap snd . M.toList . getAsts . hie_asts

mkNameCacheUpdater :: IO NameCacheUpdater
mkNameCacheUpdater = do
  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return ( initNameCache uniqSupply [] )

  nameCacheRef <- newIORef nameCache

  let update_nc f = do r <- atomicModifyIORef nameCacheRef f
                       _ <- evaluate =<< readIORef nameCacheRef
                       return r
  pure (NCU update_nc)

filterIEs :: ContextInfo -> Maybe String
filterIEs contextInfo =
  case contextInfo of
    IEThing iety -> Just (showIE iety)
    _ -> Nothing

showIE :: IEType -> String
showIE ie =
  case ie of
    Import -> "import"
    ImportAs -> "importAs"
    ImportHiding -> "importHiding"
    Export -> "export"

--filterQual
showHieType :: Show a => HieType a -> Maybe String
showHieType ht =
  case ht of
    HQualTy a b -> Just $ show a <> "," <> show b
    HTyVarTy n -> Just $ nameStableString n
    _ -> Nothing

flattenASTs :: [HieAST a] -> [HieAST a]
flattenASTs as =
  let
    foldfn a xs =
      if null $ nodeChildren a
      then
        a:xs
      else
        xs <> flattenASTs (nodeChildren a)
  in
  foldr foldfn [] as
