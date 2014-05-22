{-# LANGUAGE OverloadedStrings #-}
module Block
    ( blockSpecs
    ) where
import Test.Hspec
import Data.Text (Text)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Text.Markdown (def, MarkdownSettings(..))
import Text.Markdown.Block
import Data.Functor.Identity (runIdentity)

checkWith :: MarkdownSettings -> Text -> [Block Text] -> Expectation
checkWith ms md blocks = runIdentity (yield md $$ toBlocks ms =$ CL.consume) `shouldBe` blocks

check :: Text -> [Block Text] -> Expectation
check = checkWith def

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
        it "simple unordered" $ check
            "* foo\n\n*    bar\n\n*\t\tqux"
            [ BlockList Unordered (Right [BlockPara "foo"])
            , BlockList Unordered (Right [BlockPara "bar"])
            , BlockList Unordered (Right [BlockPara "qux"])
            ]
        it "simple ordered" $ check
            "1. foo\n\n3.    bar\n\n17.\t\tqux"
            [ BlockList Ordered (Right [BlockPara "foo"])
            , BlockList Ordered (Right [BlockPara "bar"])
            , BlockList Ordered (Right [BlockPara "qux"])
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
        it "without whitespace" $ check
            "*foo\n\n1.bar"
            [ BlockPara "*foo"
            , BlockPara "1.bar"
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
        it "require blank before blockquote" $ check
            "foo\n> bar"
            [ BlockPara "foo\n> bar" ]
        it "no blank before blockquote" $ checkWith def { msBlankBeforeBlockquote = False }
            "foo\n> bar"
            [ BlockPara "foo", BlockQuote [BlockPara "bar"]]
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
