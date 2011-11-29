{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit hiding (Test)
import Text.Markdown
import Data.Text.Lazy (Text)
import Text.Blaze.Renderer.Text (renderHtml)

check :: Text -> Text -> Assertion
check html md = html @=? renderHtml (markdown def md)

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
