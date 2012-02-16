{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit hiding (Test)
import Text.Markdown
import Data.Text.Lazy (Text, unpack, snoc)
import Text.Blaze.Renderer.Text (renderHtml)
import Control.Monad (forM_)

check :: Text -> Text -> Assertion
check html md = html @=? renderHtml (markdown def md)

check' :: Text -> Text -> Assertion
check' html md = html @=? renderHtml (markdown def { msXssProtect = False } md)

-- FIXME add quickcheck: all input is valid

main :: IO ()
main = hspecX $ do
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
            $ check "<div>foo\nbar\nbaz</div><p>paragraph</p>" close2
    describe "inline code" $ do
        it "simple"
            $ check "<p>foo <code>bar</code> baz</p>" "foo `bar` baz"
    describe "code block" $ do
        it "simple"
            $ check
                "<pre>foo\n bar\nbaz</pre>"
                "    foo\n     bar\n    baz"
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
    describe "blockquotes" $ do
        it "simple"
            $ check
                "<blockquote><p>foo</p><pre>bar</pre></blockquote>"
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

    describe "github links" $ do
        it "simple" $ check "<p><a href=\"foo.md\">bar</a></p>" "[[bar|foo]]"
        it "escaping" $ check "<p><a href=\"foo-baz-bin.md\">bar</a></p>" "[[bar|foo/baz bin]]"
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
