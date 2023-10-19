{-# LANGUAGE TemplateHaskell #-}
module Profiteur.DataFile (
  includeFile,
  readAssetFile,
  module Profiteur.DataFile.Internal
  ) where

import Profiteur.DataFile.Internal
import System.IO (Handle)
import qualified Data.ByteString       as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import Data.FileEmbed
import Control.Arrow
import qualified Language.Javascript.JQuery as JQuery
import Language.Haskell.TH (runIO)
import Data.Maybe

includeFile :: Handle -> DataType -> IO ()
includeFile h filePath = B.hPutStr h $ data' filePath
  where
    data' JQueryFile = $(embedFile =<< runIO JQuery.file)
    data' (DataFile fp) =
      fromMaybe (error $ "No such datafile: " ++ fp) $ lookup fp dataDirContents
    dataDirContents = map (first ("data/" ++)) $(embedDir "data")

readAssetFile :: DataType -> IO T.Text
readAssetFile filePath = pure $ TE.decodeUtf8 $ data' filePath
  where
    data' JQueryFile = $(embedFile =<< runIO JQuery.file)
    data' (DataFile fp) =
      fromMaybe (error $ "No such datafile: " ++ fp) $ lookup fp dataDirContents
    dataDirContents = map (first ("data/" ++)) $(embedDir "data")
