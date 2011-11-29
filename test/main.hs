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
    it "handles simple paragraphs" $ check "<p>Hello World!</p>" "Hello World!"
