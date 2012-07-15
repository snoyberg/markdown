{-# LANGUAGE OverloadedStrings #-}
module Block
    ( blockSpecs
    ) where
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit hiding (Test)
import Data.Text (Text)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Text.Markdown.Block
import Data.Functor.Identity (runIdentity)

check :: Text -> [Block Text] -> Assertion
check md blocks = runIdentity (yield md $$ toBlocks =$ CL.consume) @?= blocks

blockSpecs :: Spec
blockSpecs = do
    describe "tilde code" $ do
        it "simple" $ check
            "~~~haskell\nfoo\n\nbar\n~~~"
            [BlockCode (Just "haskell") "foo\n\nbar"]
        it "no lang" $ check
            "~~~\nfoo\n\nbar\n~~~"
            [BlockCode Nothing "foo\n\nbar"]
        it "no close" $ check
            "~~~\nfoo\n\nbar\n"
            [BlockPara " ~~~\nfoo", BlockPara "bar"]
    describe "list" $ do
        it "simple" $ check
            "* foo\n*    bar"
            [ BlockList Unordered (Left "foo")
            , BlockList Unordered (Right [BlockPara "bar"])
            ]
        it "nested" $ check
            "* foo\n*    1. bar\n     2. baz"
            [ BlockList Unordered (Left "foo")
            , BlockList Unordered (Right
                [ BlockList Ordered $ Left "bar"
                , BlockList Ordered $ Left "baz"
                ])
            ]
        it "with blank" $ check
            "*   foo\n\n    bar\n* baz"
            [ BlockList Unordered $ Right
                [ BlockPara "foo"
                , BlockPara "bar"
                ]
            , BlockList Unordered $ Left "baz"
            ]
    describe "blockquote" $ do
        it "simple" $ check
            "> foo\n>\n> * bar"
            [ BlockQuote
                [ BlockPara "foo"
                , BlockList Unordered $ Left "bar"
                ]
            ]
        it "blank" $ check
            "> foo\n\n> * bar"
            [ BlockQuote [BlockPara "foo"]
            , BlockQuote [BlockList Unordered $ Left "bar"]
            ]
    describe "indented code" $ do
        it "simple" $ check
            "    foo\n    bar\n"
            [ BlockCode Nothing "foo\nbar"
            ]
        it "blank" $ check
            "    foo\n\n    bar\n"
            [ BlockCode Nothing "foo\n\nbar"
            ]
        it "extra space" $ check
            "    foo\n\n     bar\n"
            [ BlockCode Nothing "foo\n\n bar"
            ]
    describe "html" $ do
        it "simple" $ check
            "<p>Hello world!</p>"
            [ BlockHtml "<p>Hello world!</p>"
            ]
        it "multiline" $ check
            "<p>Hello world!\n</p>"
            [ BlockHtml "<p>Hello world!\n</p>"
            ]
