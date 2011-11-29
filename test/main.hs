{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit hiding (Test)
import Text.Markdown
import Data.Text.Lazy (Text)
import Text.Blaze.Renderer.Text (renderHtml)

check :: Text -> Text -> Assertion
check html md = html @=? renderHtml (markdown def md)

main :: IO ()
main = hspecX $ describe "markdown" $ do
    it "simple paragraphs" $ check "<p>Hello World!</p>" "Hello World!"
    it "multiline paragraphs" $ check "<p>Hello\nWorld!</p>" "Hello\nWorld!"
    it "two paragraphs" $ check "<p>Hello</p><p>World!</p>" "Hello\n\nWorld!"
