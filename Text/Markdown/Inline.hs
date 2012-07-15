{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown.Inline
    ( Inline (..)
    , inlineParser
    , toInline
    ) where

import Prelude hiding (takeWhile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Data.Monoid (Monoid, mappend)

toInline :: Text -> [Inline]
toInline t =
    case parseOnly inlineParser t of
        Left s -> [InlineText $ T.pack s]
        Right is -> is

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data Inline = InlineText Text
            | InlineItalic [Inline]
            | InlineBold [Inline]
            | InlineCode Text
            -- | InlineHtml 
            | InlineLink Text (Maybe Text) [Inline] -- ^ URL, title, content
            | InlineImage Text (Maybe Text) Text -- ^ URL, title, content
    deriving (Show, Eq)

inlineParser :: Parser [Inline]
inlineParser = combine <$> many inlineAny

combine :: [Inline] -> [Inline]
combine [] = []
combine (InlineText x:InlineText y:rest) = combine (InlineText (x <> y):rest)
combine (InlineText x:rest) = InlineText x : combine rest
combine (InlineItalic x:InlineItalic y:rest) = combine (InlineItalic (x <> y):rest)
combine (InlineItalic x:rest) = InlineItalic (combine x) : combine rest
combine (InlineBold x:InlineBold y:rest) = combine (InlineBold (x <> y):rest)
combine (InlineBold x:rest) = InlineBold (combine x) : combine rest
combine (InlineCode x:InlineCode y:rest) = combine (InlineCode (x <> y):rest)
combine (InlineCode x:rest) = InlineCode x : combine rest
combine (InlineLink u t c:rest) = InlineLink u t (combine c) : combine rest
combine (InlineImage u t c:rest) = InlineImage u t c : combine rest

inlinesTill :: Text -> Parser [Inline]
inlinesTill end =
    go id
  where
    go front =
        (string end *> pure (front []))
        <|> (do
            x <- inline
            go $ front . (x:))

specials :: [Char]
specials = "*_`\\[]!"

inlineAny :: Parser Inline
inlineAny =
    inline <|> special
  where
    special = InlineText . T.singleton <$> satisfy (`elem` specials)

inline :: Parser Inline
inline =
    text
    <|> escape
    <|> paired "**" InlineBold <|> paired "__" InlineBold
    <|> paired "*" InlineItalic <|> paired "_" InlineItalic
    <|> code
    <|> link
    <|> image
  where
    text = InlineText <$> takeWhile1 (`notElem` specials)

    paired t wrap = wrap <$> do
        _ <- string t
        is <- inlinesTill t
        if null is then fail "wrapped around something missing" else return is

    code = InlineCode <$> (char '`' *> takeWhile1 (/= '`') <* char '`')

    escape = InlineText . T.singleton <$> (char '\\' *> satisfy (`elem` specials))

    link = do
        _ <- char '['
        content <- inlinesTill "]"
        _ <- char '('
        url <- takeWhile1 (`notElem` " )")
        mtitle <- (Just <$> title) <|> pure Nothing
        _ <- char ')'
        return $ InlineLink url mtitle content

    image = do
        _ <- string "!["
        content <- takeWhile (/= ']')
        _ <- string "]("
        url <- takeWhile1 (`notElem` " )")
        mtitle <- (Just <$> title) <|> pure Nothing
        _ <- char ')'
        return $ InlineImage url mtitle content

    title = space *> char '"' *> takeWhile1 (/= '"') <* char '"'
