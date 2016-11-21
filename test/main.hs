{-# LANGUAGE OverloadedStrings #-}
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 (figure)
import Test.Hspec
import Text.Markdown
import Data.Text.Lazy (Text, unpack, snoc, fromStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (forM_)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (fromMaybe)

import System.Directory (getDirectoryContents)
import System.FilePath ((</>), replaceExtension)

import Block
import Inline

check :: Text -> Text -> Expectation
check html md = renderHtml (markdown def md) `shouldBe` html

checkSet :: MarkdownSettings -> Text -> Text -> Expectation
checkSet set html md = renderHtml (markdown set md) `shouldBe` html

check' :: Text -> Text -> Expectation
check' html md = renderHtml (markdown def { msXssProtect = False } md) `shouldBe` html

checkNoNL :: Text -> Text -> Expectation
checkNoNL html md =
    f (renderHtml $ markdown def { msXssProtect = False } md) `shouldBe` f html
  where
    f = TL.filter (/= '\n')

-- FIXME add quickcheck: all input is valid

main :: IO ()
main = do
  examples <- getExamples
  gruber <- getGruber
  hspec $ do
    describe "block" blockSpecs
    describe "inline" inlineSpecs
    describe "line break" $ do
        it "is inserted for a single newline after two spaces"
            $ check "<p>Hello<br>World!</p>" "Hello  \nWorld!"
        it "is also inserted for a single CRLF after two spaces"
            $ check "<p>Hello<br>World!</p>" "Hello  \r\nWorld!"
        it "preserves quote nesting of the previous line"
            $ check "<blockquote><p>Q1<br>Q2</p></blockquote><p>P2</p>"
                    "> Q1  \nQ2\n\nP2"
        it "consumes all trailing whitespace on the previous line"
            $ check "<p>Hello<br>World!</p>" "Hello     \nWorld!"
    describe "paragraphs" $ do
        it "simple"
            $ check "<p>Hello World!</p>" "Hello World!"
        it "multiline"
            $ check "<p>Hello\nWorld!</p>" "Hello\nWorld!"
        it "multiple"
            $ check "<p>Hello</p><p>World!</p>" "Hello\n\nWorld!"
    describe "italics" $ do
        it "simple"
            $ check "<p><i>foo</i></p>" "*foo*"
        it "hanging"
            $ check "<p><i>foo</i> *</p>" "*foo* *"
        it "two"
            $ check "<p><i>foo</i> <i>bar</i></p>" "*foo* *bar*"
    describe "italics under" $ do
        it "simple"
            $ check "<p><i>foo</i></p>" "_foo_"
        it "hanging"
            $ check "<p><i>foo</i> _</p>" "_foo_ _"
        it "two"
            $ check "<p><i>foo</i> <i>bar</i></p>" "_foo_ _bar_"
    describe "bold" $ do
        it "simple"
            $ check "<p><b>foo</b></p>" "**foo**"
        it "hanging"
            $ check "<p><b>foo</b> **</p>" "**foo** **"
        it "two"
            $ check "<p><b>foo</b> <b>bar</b></p>" "**foo** **bar**"
    describe "bold under" $ do
        it "simple"
            $ check "<p><b>foo</b></p>" "__foo__"
        it "hanging"
            $ check "<p><b>foo</b> __</p>" "__foo__ __"
        it "two"
            $ check "<p><b>foo</b> <b>bar</b></p>" "__foo__ __bar__"
    describe "html" $ do
        it "simple"
            $ check "<div>Hello</div>" "<div>Hello</div>"
        it "dangerous"
            $ check "<div>Hello</div>" "<div onclick='alert(foo)'>Hello</div>"
        it "dangerous and allowed"
            $ check' "<div onclick='alert(foo)'>Hello</div>" "<div onclick='alert(foo)'>Hello</div>"

        let ml = "<div>foo\nbar\nbaz</div>"
        it "multiline" $ check ml ml

        let close = "<div>foo\nbar\nbaz"
        it "autoclose" $ check ml close

        let close2 = "<div>foo\nbar\nbaz\n\nparagraph"
        it "autoclose 2"
            $ check "<div>foo\nbar\nbaz<p>paragraph</p></div>" close2
    describe "inline code" $ do
        it "simple"
            $ check "<p>foo <code>bar</code> baz</p>" "foo `bar` baz"
    describe "code block" $ do
        it "simple"
            $ check
                "<pre><code>foo\n bar\nbaz</code></pre>"
                "    foo\n     bar\n    baz"
        it "custom renderer"
            $ checkSet
                def { msBlockCodeRenderer = (\_ (u,_) -> figure (toHtml u)) }
                "<figure>foo\n bar\nbaz</figure>"
                "```haskell\nfoo\n bar\nbaz\n```"
    describe "escaping" $ do
        it "everything"
            $ check
                "<p>*foo_bar<i>baz</i>\\`bin</p>"
                "\\*foo\\_bar_baz_\\\\\\`bin"
    describe "bullets" $ do
        it "simple"
            $ check
                "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"
                "* foo\n* bar\n* baz\n"
    describe "numbers" $ do
        it "simple"
            $ check
                "<ol><li>foo</li><li>bar</li><li>baz</li></ol>"
                "5. foo\n2. bar\n1. baz\n"
    describe "headings" $ do
        it "hashes"
            $ check
                "<h1>foo</h1><h2>bar</h2><h3>baz</h3>"
                "# foo\n\n##     bar\n\n###baz"
        it "trailing hashes"
            $ check
                "<h1>foo</h1>"
                "# foo    ####"
        it "underline"
            $ check
                "<h1>foo</h1><h2>bar</h2>"
                "foo\n=============\n\nbar\n----------------\n"
    describe "headings with ID" $ do
        let withHeadingId = def { msAddHeadingId = True }
        it "without spaces"
            $ checkSet withHeadingId
                "<h1 id=\"foo\">foo</h1><h2 id=\"bar\">bar</h2><h3 id=\"baz\">baz</h3>"
                "# foo\n\n##     bar\n\n###baz"
        it "with spaces"
            $ checkSet withHeadingId
                "<h1 id=\"executive-summary\">Executive summary</h1>"
                "# Executive summary"
        it "with special characters"
            $ checkSet withHeadingId
                "<h1 id=\"executive-summary-.-_:\">Executive summary .!@#$%^*()-_=:</h1>"
                "# Executive summary .!@#$%^*()-_=:"
    describe "blockquotes" $ do
        it "simple"
            $ check
                "<blockquote><p>foo</p><pre><code>bar</code></pre></blockquote>"
                "> foo\n>\n>     bar"
    describe "links" $ do
        it "simple" $ check "<p><a href=\"foo\">bar</a></p>" "[bar](foo)"
        it "title" $ check
            "<p><a href=\"foo\" title=\"baz\">bar</a></p>"
            "[bar](foo \"baz\")"
        it "escaped href" $ check
            "<p><a href=\"foo)\" title=\"baz\">bar</a></p>"
            "[bar](foo\\) \"baz\")"
        it "escaped title" $ check
            "<p><a href=\"foo)\" title=\"baz&quot;\">bar</a></p>"
            "[bar](foo\\) \"baz\\\"\")"
        it "inside a paragraph" $ check
            "<p>Hello <a href=\"foo\">bar</a> World</p>"
            "Hello [bar](foo) World"
        it "not a link" $ check
            "<p>Not a [ link</p>"
            "Not a [ link"
        it "new tab" $ checkSet def { msLinkNewTab = True }
            "<p><a href=\"foo\" target=\"_blank\">bar</a></p>"
            "[bar](foo)"

    {-
    describe "github links" $ do
        it "simple" $ check "<p><a href=\"foo\">bar</a></p>" "[[bar|foo]]"
        it "no link text" $ check "<p><a href=\"foo\">foo</a></p>" "[[foo]]"
        it "escaping" $ check "<p><a href=\"foo-baz-bin\">bar</a></p>" "[[bar|foo/baz bin]]"
        it "inside a list" $ check "<ul><li><a href=\"foo\">foo</a></li></ul>" "* [[foo]]"
    -}

    describe "images" $ do
        it "simple" $ check
            "<p><img src=\"http://link.to/image.jpg\" alt=\"foo\"></p>"
            "![foo](http://link.to/image.jpg)"
        it "title" $ check
            "<p><img src=\"http://link.to/image.jpg\" alt=\"foo\" title=\"bar\"></p>"
            "![foo](http://link.to/image.jpg \"bar\")"
        it "inside a paragraph" $ check
            "<p>Hello <img src=\"http://link.to/image.jpg\" alt=\"foo\"> World</p>"
            "Hello ![foo](http://link.to/image.jpg) World"
        it "not an image" $ check
            "<p>Not an ![ image</p>"
            "Not an ![ image"

    describe "rules" $ do
        let options = concatMap (\t -> [t, snoc t '\n'])
                [ "* * *"
                , "***"
                , "*****"
                , "- - -"
                , "---------------------------------------"
                , "----------------------------------"
                ]
        forM_ options $ \o -> it (unpack o) $ check "<hr>" o

    describe "html" $ do
        it "inline" $ check "<p>foo<br>bar</p>" "foo<br>bar"
        it "inline xss" $ check "<p>foo<br>bar</p>" "foo<br onclick='evil'>bar"
        it "block" $ check "<div>hello world</div>" "<div>hello world</div>"
        it "block xss" $ check "alert('evil')" "<script>alert('evil')</script>"
        it "should be escaped" $ check "<p>1 &lt; 2</p>" "1 < 2"
        it "standalone" $ checkSet
            def { msStandaloneHtml = Set.fromList ["<hidden>", "</hidden>"], msXssProtect = False }
            "<hidden><pre><code class=\"haskell\">foo\nbar</code></pre></hidden>"
            "<hidden>\n```haskell\nfoo\nbar\n```\n</hidden>\n"
    describe "fencing" $ do
        it "custom fencing" $ checkSet
            def
                { msFencedHandlers = Map.union
                    (htmlFencedHandler "@@@" (\clazz -> T.concat ["<article class=\"", clazz, "\">"]) (const "</article>"))
                    (msFencedHandlers def)
                }
            "<article class=\"someclass\"><p>foo</p><blockquote><p>bar</p></blockquote></article>"
            "@@@ someclass\nfoo\n\n> bar\n@@@"
    describe "footnotes" $ do
        it "inline" $
            check "<p><a href=\"#footnote-1\" id=\"ref-1\">[1]</a>hello</p>"
                  "{1}hello"
        it "references" $
            check "<p><a href=\"#ref-1\" id=\"footnote-1\">[1]</a>hello</p>"
                  "{^1}hello"
    describe "examples" $ sequence_ examples
    describe "John Gruber's test suite" $ sequence_ gruber

    it "comments without spaces #22" $
        check "<!--<>-->" "<!--<>-->"

    describe "no follow" $ do
        it "external 1" $ checkSet (setNoFollowExternal def)
            "<p><a href=\"http://example.com\" rel=\"nofollow\">example</a></p>"
            "[example](http://example.com)"
        it "external 2" $ checkSet (setNoFollowExternal def)
            "<p><a href=\"//example.com\" rel=\"nofollow\">example</a></p>"
            "[example](//example.com)"
        it "internal" $ checkSet (setNoFollowExternal def)
            "<p><a href=\"/foo\">example</a></p>"
            "[example](/foo)"

getExamples :: IO [Spec]
getExamples = do
    files <- getDirectoryContents dir
    mapM go $ filter (".md" `isSuffixOf`) files
  where
    dir = "test/examples"
    go basename = do
        let fp = dir </> basename
        input <- TIO.readFile fp
        output <- TIO.readFile $ replaceExtension fp "html"
        let (checker, stripper)
                | "-spec" `isInfixOf` fp = (check', dropFinalLF)
                | otherwise = (check, T.strip)

        return $ it basename $ checker (fromStrict $ stripper output) (fromStrict input)

    dropFinalLF t = fromMaybe t $ T.stripSuffix "\n" t

getGruber :: IO [Spec]
getGruber = do
    files <- getDirectoryContents dir
    mapM go $ filter (".text" `isSuffixOf`) files
  where
    dir = "test/Tests"
    go basename = do
        let fp = dir </> basename
        input <- TIO.readFile fp
        output <- TIO.readFile $ replaceExtension fp "html"
        return $ it basename $ checkNoNL (fromStrict $ T.strip output) (fromStrict input)
