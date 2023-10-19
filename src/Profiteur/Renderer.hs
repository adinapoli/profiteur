{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Profiteur.Renderer (
  -- * Types
    CssAssets(..)
  , JsAssets(..)
  , DataType(..)

  , includeJs
  , includeCss
  , readAssetFile

  , encodedProfToHtml
  , reportToHtml
  , jsonReportToHtml
  ) where

import           Data.String
import           Profiteur.Core
import           Profiteur.DataFile
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BC8
import qualified Data.Text         as T
import           System.FilePath            (takeBaseName)
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

newtype CssAssets = CssAssets { _CssAssets :: [Html] }

newtype JsAssets = JsAssets { _JsAssets :: [Html] }

includeJs :: DataType -> IO Html
includeJs file = do
  fileContent <- T.unpack <$> readAssetFile file
  pure $ H.script ! A.type_ "text/javascript" $ preEscapedToHtml fileContent

includeCss :: DataType -> IO Html
includeCss file = do
  fileContent <- T.unpack <$> readAssetFile file
  pure $ H.style $ fromString fileContent

--------------------------------------------------------------------------------
withReportHeader :: String -> Html -> Html
withReportHeader profFile innerHtml =
    H.head $ do
      H.meta ! charset "UTF-8"
      H.title $ fromString profTitle
      innerHtml
  where
    profTitle = takeBaseName profFile

encodedProfToHtml :: Aeson.Value -> Html
encodedProfToHtml json =
  let jsonProf = BC8.toStrict $ "var $prof = " <> Aeson.encode json <> ";"
  in H.script ! A.type_ "text/javascript" $ fromString (BC8.unpack jsonProf)

reportBody :: T.Text -> Html
reportBody bodyContent =
  H.body $ H.preEscapedToHtml $ T.unpack bodyContent

reportToHtml :: String
             -> JsAssets
             -> CssAssets
             -> T.Text
             -> NodeMap
             -> Html
reportToHtml profFile jsAssets cssAssets bodyContent prof =
  jsonReportToHtml profFile jsAssets cssAssets bodyContent (Aeson.toJSON prof)

jsonReportToHtml :: String
                 -> JsAssets
                 -> CssAssets
                 -> T.Text
                 -> Aeson.Value
                 -> Html
jsonReportToHtml profFile (JsAssets jsAssets) (CssAssets cssAssets) bodyContent jsonProf = do
  H.docTypeHtml $ do
    withReportHeader profFile $ do
      encodedProfToHtml jsonProf
      mconcat cssAssets
      mconcat jsAssets
    reportBody bodyContent
