{-# LANGUAGE OverloadedStrings #-}

module Static.Html where

import Prelude hiding (head, id, div)
import Data.ByteString.Builder (Builder)
import Data.String (fromString)
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

html :: FilePath -> FilePath -> Builder
html js css = renderHtmlBuilder . docTypeHtml $ do
    head $ do
        title "Eclogues Jobs"
        meta ! charset "utf-8"
        link ! rel "stylesheet" ! href (fromString css)
        script $ "window.eclogues_hostname = 'localhost';window.eclogues_port = 8000;"
    body $ do
        div ! id "app-container" $ empty
        script ! src (fromString js) $ empty

empty :: (Applicative f) => f ()
empty = pure ()
