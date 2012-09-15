{-# LANGUAGE OverloadedStrings #-}
module Inline
    ( inlineSpecs
    ) where

import Test.Hspec
import Text.Markdown.Inline
import Data.Text (Text)
import Data.Monoid (mempty)

check :: Text -> [Inline] -> Expectation
check md ins = toInline mempty md `shouldBe` ins

inlineSpecs :: Spec
inlineSpecs = do
    describe "raw text" $ do
        it "simple"
            $ check "raw text" [InlineText "raw text"]
        it "multiline"
            $ check "raw\ntext" [InlineText "raw\ntext"]
    describe "italic" $ do
        it "asterisk"
            $ check "raw *text*" [InlineText "raw ", InlineItalic [InlineText "text"]]
        it "underline"
            $ check "raw _text_" [InlineText "raw ", InlineItalic [InlineText "text"]]
        it "multiline"
            $ check "*raw\ntext*" [InlineItalic [InlineText "raw\ntext"]]
        it "mismatched"
            $ check "*foo* *bar" [InlineItalic [InlineText "foo"], InlineText " *bar"]
    describe "bold" $ do
        it "asterisk"
            $ check "raw **text**" [InlineText "raw ", InlineBold [InlineText "text"]]
        it "underline"
            $ check "raw __text__" [InlineText "raw ", InlineBold [InlineText "text"]]
        it "multiline"
            $ check "**raw\ntext**" [InlineBold [InlineText "raw\ntext"]]
        it "mismatched"
            $ check "**foo** *bar" [InlineBold [InlineText "foo"], InlineText " *bar"]
    describe "nested" $ do
        it "bold inside italic"
            $ check "*i __ib__ i*" [InlineItalic [InlineText "i ", InlineBold [InlineText "ib"], InlineText " i"]]
        it "bold inside italic swap"
            $ check "_i **ib** i_" [InlineItalic [InlineText "i ", InlineBold [InlineText "ib"], InlineText " i"]]
        it "italic inside bold"
            $ check "**b _ib_ b**" [InlineBold [InlineText "b ", InlineItalic [InlineText "ib"], InlineText " b"]]
        it "italic inside bold swap"
            $ check "__b *ib* b__" [InlineBold [InlineText "b ", InlineItalic [InlineText "ib"], InlineText " b"]]
    describe "code" $ do
        it "takes all characters"
            $ check "`foo*__*bar` baz`"
                [ InlineCode "foo*__*bar"
                , InlineText " baz`"
                ]
    describe "escaping" $ do
        it "asterisk"
            $ check "\\*foo*\\\\" [InlineText "*foo*\\"]
    describe "links" $ do
        it "simple" $ check "[bar](foo)" [InlineLink "foo" Nothing [InlineText "bar"]]
        it "title" $ check
            "[bar](foo \"baz\")"
            [InlineLink "foo" (Just "baz") [InlineText "bar"]]
            {-
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
            -}
