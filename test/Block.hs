{-# LANGUAGE OverloadedStrings #-}
module Block
    ( blockSpecs
    ) where
import Test.Hspec
import Data.Text (Text)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Text.Markdown (def)
import Text.Markdown.Block
import Data.Functor.Identity (runIdentity)

check :: Text -> [Block Text] -> Expectation
check md blocks = runIdentity (yield md $$ toBlocks def =$ CL.consume) `shouldBe` blocks

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
            "* foo\n\n*    bar\n\n"
            [ BlockList Unordered (Right [BlockPara "foo"])
            , BlockList Unordered (Right [BlockPara "bar"])
            ]
        it "nested" $ check
            "* foo\n* \n    1. bar\n    2. baz"
            [ BlockList Unordered (Left "foo")
            , BlockList Unordered (Right
                [ BlockList Ordered $ Left "bar"
                , BlockList Ordered $ Left "baz"
                ])
            ]
        it "with blank" $ check
            "*   foo\n\n    bar\n\n* baz"
            [ BlockList Unordered $ Right
                [ BlockPara "foo"
                , BlockPara "bar"
                ]
            , BlockList Unordered $ Right
                [ BlockPara "baz"
                ]
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
