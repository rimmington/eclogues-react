#!/usr/bin/env stack
-- stack --resolver=lts-6.21 --compiler=ghc-7.10.3 runghc --package shake --package blaze-html --package stringsearch --package mtl
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Static.Html (html)

import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Binary (Binary)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Search (replace)
import Data.ByteString.Builder
import Data.Char (isSpace)
import Data.Hashable (Hashable)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Development.Shake hiding ((*>))
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.IO (IOMode (..), withFile)

newtype InstallRoot = InstallRoot ()
                    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/index-dev.html"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    installRoot <- addOracle $ \(InstallRoot _) -> do
        Stdout out <- cmd "stack path --local-install-root"
        pure $ dropWhileEnd isSpace out

    "_build/all.js" %> \out -> do
        need =<< getDirectoryFiles "" ["app//*.hs", "*.cabal", "stack.yaml"]
        unit $ cmd "stack test"
        dir <- installRoot $ InstallRoot ()
        let fp = dir </> "bin/eclogues-react.jsexe/all.js"
        wrapJSFile ["React", "ReactDOM"] fp out
        unit $ cmd "sed -i" "s/goog.provide.*//" out
        unit $ cmd "sed -i" "s/goog.require.*//" out

    "_build/all.min.js" %> ccjs "_build/all.js" ["_build/react-externs.js"]

    "_build/react.js" %> downloadFile "https://unpkg.com/react@15.3.2/dist/react.js"
    "_build/react-dom.js" %> downloadFile "https://unpkg.com/react-dom@15.3.2/dist/react-dom.js"
    "_build/react-externs.js" %> downloadFile "https://raw.githubusercontent.com/steida/react-externs/0fac28595bd1fa7b95d0ed302a04003d9ab4fb69/externs.js"
    "_build/react.min.js" %> downloadFile "https://unpkg.com/react@15.3.2/dist/react.min.js"
    "_build/react-dom.min.js" %> downloadFile "https://unpkg.com/react-dom@15.3.2/dist/react-dom.min.js"

    "_build/uber.js" %> \out ->
        writeBuilder out . foldMap lazyByteString <=< lReadFiles $
            ["_build/react.js", "_build/react-dom.js", "_build/all.js"]

    -- TODO: wrap all? + hide 'window'
    "_build/uber.min.js" %> \out -> do
        writeBuilder out . foldMap lazyByteString <=< lReadFiles $
            ["_build/react.min.js", "_build/react-dom.min.js", "_build/all.min.js"]

    "_build/glyphicons-halflings-regular.woff" %> downloadFile "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/fonts/glyphicons-halflings-regular.woff"

    "_build/bootstrap.min.css" %> \out -> do
        let local = replace (pack "../fonts/") (pack "./")
        need ["_build/glyphicons-halflings-regular.woff"]
        writeBuilder out . foldMap lazyByteString <=< traverse (fmap local . download) $
            [ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
            , "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css" ]

    "_build/uber.css" %> \out ->
        writeBuilder out . foldMap lazyByteString <=< lReadFiles $
            ["_build/bootstrap.min.css"]

    "_build/uber.min.css" %> copyFile' "_build/uber.css"

    "_build/index-dev.html" %> \out -> do
        need ["_build/uber.js", "_build/uber.css", "Static/Html.hs"]
        writeBuilder out $ html "uber.js" "uber.css"

    "_build/index.html" %> \out -> do
        need ["_build/uber.min.js", "_build/uber.min.css", "Static/Html.hs"]
        writeBuilder out $ html "uber.min.js" "uber.min.css"

    "_build/*.gz" %> \out -> do
        let fp = out -<.> ""
        need [fp]
        cmd "zopfli -i1000" fp

ccjs :: FilePath -> [FilePath] -> FilePath -> Action ()
ccjs fp externs out = do
    need $ fp : externs
    Stdout minjs <- cmd (EchoStderr False) "ccjs" fp "--compilation_level=ADVANCED_OPTIMIZATIONS" (concat $ ("--externs=" ++) <$> externs)
    liftIO $ L.writeFile out minjs

lWriteFile :: (MonadIO m) => FilePath -> L.ByteString -> m ()
lWriteFile fp = liftIO . L.writeFile fp

lReadFiles :: [FilePath] -> Action [L.ByteString]
lReadFiles fps = need fps *> traverse lReadFile' fps

lReadFile' :: (MonadIO m) => FilePath -> m L.ByteString
lReadFile' = liftIO . L.readFile

wrapJSFile :: (MonadIO m) => [String] -> FilePath -> FilePath -> m ()
wrapJSFile props fp out = writeBuilder out =<< wrapJS props . lazyByteString <$> lReadFile' fp

wrapJS :: [String] -> Builder -> Builder
wrapJS props code =
       stringUtf8 "(function(global"
    <> mconcat ((charUtf8 ',' <>) . stringUtf8 <$> props)
    <> stringUtf8 ") {\n"
    <> code
    <> stringUtf8 "})(window"
    <> mconcat ((\s -> stringUtf8 ",window['" <> stringUtf8 s <> stringUtf8 "']") <$> props)
    <> stringUtf8 ");"

writeBuilder :: (MonadIO m) => FilePath -> Builder -> m ()
writeBuilder fp b = liftIO . withFile fp WriteMode $ \h -> hPutBuilder h b

downloadFile :: String -> FilePath -> Action ()
downloadFile url out = lWriteFile out =<< download url

download :: String -> Action L.ByteString
download = fmap fromStdout . cmd (EchoStderr False) "wget -O -"
