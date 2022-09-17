{-# LANGUAGE TemplateHaskell #-}
module Config.Defaults
  ( packageFile
  , initialModulintFile
  ) where

import qualified Data.ByteString as BS
import qualified Data.FileEmbed as FileEmbed

packageFile :: BS.ByteString
packageFile =
  $(FileEmbed.embedFile "app/Config/package.dhall")

initialModulintFile :: BS.ByteString
initialModulintFile =
  $(FileEmbed.embedFile "app/Config/modulint.dhall")
