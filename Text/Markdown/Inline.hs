{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
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
import qualified Data.Map as Map
import Text.Markdown.Types (Inline(..))

type RefMap = Map.Map Text Text

toInline :: RefMap -> Text -> [Inline]
toInline refmap t =
    case parseOnly (inlineParser refmap) t of
        Left s -> [InlineText $ T.pack s]
        Right is -> is

(<>) :: Monoid m => m -> m -> m
(<>) = mappend


inlineParser :: RefMap -> Parser [Inline]
inlineParser = fmap combine . many . inlineAny

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
combine (InlineHtml t:rest) = InlineHtml t : combine rest
combine (InlineFootnote x:rest) = InlineFootnote x : combine rest
combine (InlineFootnoteRef x:rest) = InlineFootnoteRef x : combine rest

specials :: [Char]
specials = "*_`\\[]!<&{}"

inlineAny :: RefMap -> Parser Inline
inlineAny refs =
    inline refs <|> special
  where
    special = InlineText . T.singleton <$> satisfy (`elem` specials)

inline :: RefMap -> Parser Inline
inline refs =
    text
    <|> escape
    <|> footnote
    <|> footnoteRef
    <|> paired "**" InlineBold <|> paired "__" InlineBold
    <|> paired "*" InlineItalic <|> paired "_" InlineItalic
    <|> doubleCodeSpace <|> doubleCode <|> code
    <|> link
    <|> image
    <|> autoLink
    <|> html
    <|> entity
  where
    inlinesTill :: Text -> Parser [Inline]
    inlinesTill end =
        go id
      where
        go front =
            (string end *> pure (front []))
            <|> (do
                x <- inlineAny refs
                go $ front . (x:))

    text = InlineText <$> takeWhile1 (`notElem` specials)

    paired t wrap = wrap <$> do
        _ <- string t
        is <- inlinesTill t
        if null is then fail "wrapped around something missing" else return is

    doubleCodeSpace = InlineCode . T.pack <$> (string "`` " *> manyTill anyChar (string " ``"))
    doubleCode = InlineCode . T.pack <$> (string "``" *> manyTill anyChar (string "``"))
    code = InlineCode <$> (char '`' *> takeWhile1 (/= '`') <* char '`')

    footnoteRef = InlineFootnoteRef <$> (char '{' *> decimal <* char '}')
    footnote = InlineFootnote <$> (string "{^" *> decimal <* char '}')

    escape = InlineText . T.singleton <$>
        (char '\\' *> satisfy (`elem` ("\\`*_{}[]()#+-.!>" :: String)))

    takeBalancedBrackets =
        T.pack <$> go (0 :: Int)
      where
        go i = do
            c <- anyChar
            case c of
                '[' -> (c:) <$> go (i + 1)
                ']'
                    | i == 0 -> return []
                    | otherwise -> (c:) <$> go (i - 1)
                _ -> (c:) <$> go i

    parseUrl = fixUrl . T.pack <$> parseUrl' (0 :: Int)

    parseUrl' level
        | level > 0 = do
            c <- anyChar
            let level'
                    | c == ')' = level - 1
                    | otherwise = level
            c' <-
                if c == '\\'
                    then anyChar
                    else return c
            cs <- parseUrl' level'
            return $ c' : cs
        | otherwise = (do
            c <- hrefChar
            if c == '('
                then (c:) <$> parseUrl' 1
                else (c:) <$> parseUrl' 0) <|> return []

    parseUrlTitle defRef = parseUrlTitleInline <|> parseUrlTitleRef defRef

    parseUrlTitleInside endTitle = do
        url <- parseUrl
        mtitle <- (Just <$> title) <|> (skipSpace >> endTitle >> pure Nothing)
        return (url, mtitle)
      where
        title = do
            _ <- space
            skipSpace
            _ <- char '"'
            t <- T.stripEnd . T.pack <$> go
            return $
                if not (T.null t) && T.last t == '"'
                    then T.init t
                    else t
          where
            go =  (char '\\' *> anyChar >>= \c -> (c:) <$> go)
              <|> (endTitle *> return [])
              <|> (anyChar >>= \c -> (c:) <$> go)

    parseUrlTitleInline = char '(' *> parseUrlTitleInside (char ')')

    parseUrlTitleRef defRef = do
        ref' <- (skipSpace *> char '[' *> takeWhile (/= ']') <* char ']') <|> return ""
        let ref = if T.null ref' then defRef else ref'
        case Map.lookup (T.unwords $ T.words ref) refs of
            Nothing -> fail "ref not found"
            Just t -> either fail return $ parseOnly (parseUrlTitleInside endOfInput) t

    link = do
        _ <- char '['
        rawContent <- takeBalancedBrackets
        content <- either fail return $ parseOnly (inlineParser refs) rawContent
        (url, mtitle) <- parseUrlTitle rawContent
        return $ InlineLink url mtitle content

    image = do
        _ <- string "!["
        content <- takeBalancedBrackets
        (url, mtitle) <- parseUrlTitle content
        return $ InlineImage url mtitle content

    fixUrl t
        | T.length t > 2 && T.head t == '<' && T.last t == '>' = T.init $ T.tail t
        | otherwise = t

    autoLink = do
        _ <- char '<'
        a <- string "http:" <|> string "https:"
        b <- takeWhile1 (/= '>')
        _ <- char '>'
        let url = a `T.append` b
        return $ InlineLink url Nothing [InlineText url]

    html = do
        c <- char '<'
        t <- takeWhile1 (\x -> ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z') || x == '/')
        if T.null t
            then fail "invalid tag"
            else do
                t2 <- takeWhile (/= '>')
                c2 <- char '>'
                return $ InlineHtml $ T.concat
                    [ T.singleton c
                    , t
                    , t2
                    , T.singleton c2
                    ]

    entity =
            rawent "&lt;"
        <|> rawent "&gt;"
        <|> rawent "&amp;"
        <|> rawent "&quot;"
        <|> rawent "&apos;"
        <|> decEnt
        <|> hexEnt

    rawent t = InlineHtml <$> string t

    decEnt = do
        s <- string "&#"
        t <- takeWhile1 $ \x -> ('0' <= x && x <= '9')
        c <- char ';'
        return $ InlineHtml $ T.concat
            [ s
            , t
            , T.singleton c
            ]

    hexEnt = do
        s <- string "&#x" <|> string "&#X"
        t <- takeWhile1 $ \x -> ('0' <= x && x <= '9') || ('A' <= x && x <= 'F') || ('a' <= x && x <= 'f')
        c <- char ';'
        return $ InlineHtml $ T.concat
            [ s
            , t
            , T.singleton c
            ]

hrefChar :: Parser Char
hrefChar = (char '\\' *> anyChar) <|> satisfy (notInClass " )")
