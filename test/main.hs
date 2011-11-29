{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit hiding (Test)
import Text.Markdown
import Data.Text.Lazy (Text)
import Text.Blaze.Renderer.Text (renderHtml)

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
