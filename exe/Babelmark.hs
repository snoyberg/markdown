{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | See: http://johnmacfarlane.net/babelmark2/faq.html#how-can-i-add-my-markdown-implementation-to-babelmark-2
module Babelmark where

import           BasicPrelude
import           Control.Monad                         (join)
import           Data.Aeson                            (encode, object, (.=))
import           Data.Text                             (pack)
import           Data.Text.Encoding                    (decodeUtf8With)
import           Data.Text.Encoding.Error              (lenientDecode)
import           Data.Text.Lazy                        (fromStrict)
import           Distribution.Package                  (pkgVersion)
import           Distribution.PackageDescription       (package,
                                                        packageDescription)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Text                     (display)
import qualified Language.Haskell.TH.Syntax            as TH
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp              (run)
import           System.Environment                    (getEnv)
import           Text.Blaze.Html                       (Html)
import qualified Text.Blaze.Html.Renderer.Text         as T
import           Text.Blaze.Html.Renderer.Utf8         (renderHtml)
import           Text.Hamlet                           (shamlet)
import           Text.Markdown                         (def, markdown)

version :: Text
version = pack $(do
    gpd <- TH.qRunIO $ readPackageDescription minBound "markdown.cabal"
    TH.lift $ display $ pkgVersion $ package $ packageDescription gpd)

main :: IO ()
main = do
    putStrLn $ "Launching Babelmark server, markdown version " ++ version
    port <- read . pack <$> getEnv "PORT"
    run port app

app :: Application
app req =
    return $ case fmap (decodeUtf8With lenientDecode) $ join $ lookup "text" $ queryString req of
        Nothing -> responseLBS status200 [("Content-Type", "text/html")] $ renderHtml form
        Just bs -> responseLBS status200 [("Content-Type", "application/json")] $ encode $ object
            [ "name" .= ("Haskell markdown package" :: Text)
            , "html" .= T.renderHtml (markdown def (fromStrict bs))
            , "version" .= version
            ]

form :: Html
form = [shamlet|
$doctype 5
<html>
    <head>
        <title>Markdown Experimenter
    <body>
        <form>
            <textarea name=text>
            <input type=submit>
|]
