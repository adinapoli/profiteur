module Profiteur.DataFile (
  includeFile,
  readAssetFile,
  module Profiteur.DataFile.Internal
  ) where

import           Paths_profiteur            (getDataFileName)
import System.IO (Handle)
import qualified Data.ByteString.Lazy       as BL
import qualified Language.Javascript.JQuery as JQuery
import Profiteur.DataFile.Internal
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

includeFile :: Handle -> DataType -> IO ()
includeFile h JQueryFile =
    BL.hPutStr h =<< BL.readFile =<< JQuery.file
includeFile h (DataFile filePath) =
    BL.hPutStr h =<< BL.readFile =<< getDataFileName filePath

readAssetFile :: DataType -> IO T.Text
readAssetFile JQueryFile =
    TIO.readFile =<< JQuery.file
readAssetFile (DataFile filePath) =
    TIO.readFile =<< getDataFileName filePath

