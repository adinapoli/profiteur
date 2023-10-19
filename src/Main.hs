--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8      as BC8
import qualified Data.Text.Lazy.IO          as TL
import           Control.Monad              (forM)
import           Data.Version               (showVersion)
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)
import qualified System.IO                  as IO
import Text.Blaze.Html.Renderer.Utf8        (renderHtml)


--------------------------------------------------------------------------------
import           Paths_profiteur            (version)
import           Profiteur.Core
import           Profiteur.Parser
import           Profiteur.DataFile
import           Profiteur.Renderer


--------------------------------------------------------------------------------
writeReport :: IO.Handle -> String -> NodeMap -> IO ()
writeReport h profFile prof = do

    let jsAssetsFiles = [
            JQueryFile
          , "data/js/unicode.js"
          , "data/js/model.js"
          , "data/js/resizing-canvas.js"
          , "data/js/node.js"
          , "data/js/selection.js"
          , "data/js/zoom.js"
          , "data/js/details.js"
          , "data/js/sorting.js"
          , "data/js/tree-map.js"
          , "data/js/tree-browser.js"
          , "data/js/main.js"
          ]
    jsAssets  <- JsAssets  <$> forM jsAssetsFiles includeJs
    cssAssets <- CssAssets <$> forM ["data/css/main.css"] includeCss
    bodyContent <- readAssetFile "data/html/body.html"

    BC8.hPutStrLn h $ BC8.toStrict $ renderHtml $
      reportToHtml profFile jsAssets cssAssets bodyContent prof

--------------------------------------------------------------------------------
makeReport :: IO.Handle -> FilePath -> IO ()
makeReport h profFile = do
    profOrErr <- decode <$> TL.readFile profFile
    case profOrErr of
        Right prof ->
            writeReport h profFile $ nodeMapFromCostCentre prof
        Left err   -> do
            putStrLnErr $ profFile ++ ": " ++ err
            exitFailure

--------------------------------------------------------------------------------
putStrLnErr :: String -> IO ()
putStrLnErr = IO.hPutStrLn IO.stderr

--------------------------------------------------------------------------------
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    case args of
        _ | "--version" `elem` args ->
            putStrLnErr (showVersion version)
        [profFile] ->
            let htmlFile = profFile ++ ".html"
            in IO.withBinaryFile htmlFile IO.WriteMode $ \h ->
                  makeReport h profFile
        [profFile, "-"] ->
            makeReport IO.stdout profFile
        [profFile, htmlFile] ->
            IO.withBinaryFile htmlFile IO.WriteMode $ \h ->
                makeReport h profFile
        _ -> do
            putStrLnErr $ "Usage: " ++ progName ++ " <prof file> [<output file>]"
            putStrLnErr   "   <output file> \"-\" means STDOUT"
            exitFailure
