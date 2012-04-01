{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown
    ( MarkdownSettings
    , msXssProtect
    , def
    , markdown
    , Markdown (..)
    ) where

import Prelude hiding (sequence, takeWhile)
import Data.Default (Default (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze (Html, ToHtml, toHtml, toValue, preEscapedText, (!))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Data.Functor.Identity (runIdentity)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Attoparsec.Text
    ( Parser, takeWhile, string, skip, char, parseOnly, try
    , takeWhile1, notInClass, inClass, satisfy
    , skipSpace, anyChar, endOfInput, decimal
    )
import Data.Attoparsec.Combinator (many1)
import Control.Applicative ((<$>), (<|>), optional, (*>), (<*), many)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Control.Monad (unless, guard)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Data.List (intersperse)
import Data.Char (isSpace, isAlpha)
import Control.Monad.Trans.Resource (runExceptionT_)
import Text.Blaze.Renderer.Text (renderHtml)

data MarkdownSettings = MarkdownSettings
    { msXssProtect :: Bool
    }

instance Default MarkdownSettings where
    def = MarkdownSettings
        { msXssProtect = True
        }

newtype Markdown = Markdown TL.Text

instance ToHtml Markdown where
    toHtml (Markdown t) = markdown def t

markdown :: MarkdownSettings -> TL.Text -> Html
markdown ms tl =
            runIdentity
          $ runExceptionT_
          $ CL.sourceList (TL.toChunks $ TL.filter (/= '\r') tl)
       C.$$ markdownIter ms

markdownIter :: C.MonadThrow m
             => MarkdownSettings
             -> C.Sink Text m Html
markdownIter ms = markdownEnum ms C.=$ CL.fold mappend mempty

markdownEnum :: C.MonadThrow m
             => MarkdownSettings
             -> C.Conduit Text m Html
markdownEnum ms = C.sequenceSink () $ \() ->
    C.Emit () . return <$> (sinkParser $ parser ms)

nonEmptyLines :: Parser [Html]
nonEmptyLines = map line <$> nonEmptyLinesText

nonEmptyLinesText :: Parser [Text]
nonEmptyLinesText =
    go id
  where
    go :: ([Text] -> [Text]) -> Parser [Text]
    go front = do
        l <- takeWhile (/= '\n')
        _ <- optional $ skip (== '\n')
        if T.null l then return (front []) else go $ front . (l:)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

parser :: MarkdownSettings -> Parser Html
parser ms =
    html
    <|> rules
    <|> hashheads <|> underheads
    <|> codeblock
    <|> blockquote
    <|> bullets
    <|> numbers
    <|> para
  where
    html = do
        c <- char '<'
        ls' <- nonEmptyLinesText
        let ls =
                case ls' of
                    a:b -> T.cons c a:b
                    [] -> [T.singleton c]
        let t = T.intercalate "\n" ls
        let t' = if msXssProtect ms then sanitizeBalance t else t
        return $ preEscapedText t'

    rules =
            (string "* * *\n" *> return H.hr)
        <|> (try $ string "* * *" *> endOfInput *> return H.hr)
        <|> (string "***\n" *> return H.hr)
        <|> (try $ string "***" *> endOfInput *> return H.hr)
        <|> (string "*****\n" *> return H.hr)
        <|> (try $ string "*****" *> endOfInput *> return H.hr)
        <|> (string "- - -\n" *> return H.hr)
        <|> (try $ string "- - -" *> endOfInput *> return H.hr)
        <|> (try $ do
            x <- takeWhile1 (== '-')
            char' '\n' <|> endOfInput
            if T.length x >= 5
                then return H.hr
                else fail "not enough dashes"
                    )

    para = do
        ls <- nonEmptyLines
        return $ if (null ls)
            then mempty
            else H.p $ foldr1
                    (\a b -> a <> preEscapedText "\n" <> b) ls

    hashheads = do
        _c <- char '#'
        x <- takeWhile (== '#')
        skipSpace
        l <- takeWhile (/= '\n')
        let h =
                case T.length x of
                    0 -> H.h1
                    1 -> H.h2
                    2 -> H.h3
                    3 -> H.h4
                    4 -> H.h5
                    _ -> H.h6
        return $ h $ line $ T.dropWhileEnd isSpace $ T.dropWhileEnd (== '#') l

    underheads = try $ do
        x <- takeWhile (/= '\n')
        _ <- char '\n'
        y <- satisfy $ inClass "=-"
        ys <- takeWhile (== y)
        unless (T.length ys >= 2) $ fail "Not enough unders"
        _ <- char '\n'
        let l = line x
        return $ (if y == '=' then H.h1 else H.h2) l

    codeblock = H.pre . H.code . mconcat . map toHtml . intersperse "\n"
            <$> many1 indentedLine

    blockquote = H.blockquote . markdown ms . TL.fromChunks . intersperse "\n"
             <$> many1 blockedLine

    bullets = H.ul . mconcat <$> many1 (bullet ms)
    numbers = H.ol . mconcat <$> many1 (number ms)

string' :: Text -> Parser ()
string' s = string s *> return ()

bulletStart :: Parser ()
bulletStart = string' "* " <|> string' "- " <|> string' "+ "

bullet :: MarkdownSettings -> Parser Html
bullet _ms = do
    bulletStart
    content <- itemContent
    return $ H.li content

numberStart :: Parser ()
numberStart =
    try $ decimal' *> satisfy (inClass ".)") *> char' ' '
  where
    decimal' :: Parser Int
    decimal' = decimal

number :: MarkdownSettings -> Parser Html
number _ms = do
    numberStart
    content <- itemContent
    return $ H.li content

itemContent :: Parser Html
itemContent = do
    t <- takeWhile (/= '\n') <* (optional $ char' '\n')
    return $ line t

indentedLine :: Parser Text
indentedLine = string "    " *> takeWhile (/= '\n') <* (optional $ char '\n')

blockedLine :: Parser Text
blockedLine = (string ">\n" *> return "") <|>
              (string "> " *> takeWhile (/= '\n') <* (optional $ char '\n'))

line :: Text -> Html
line t =
    preEscapedText $ sanitizeBalance $ TL.toStrict $ renderHtml h
  where
    h = either error mconcat $ parseOnly (many phrase) t

phrase :: Parser Html
phrase =
    boldU <|> italicU <|> underscore <|>
    bold <|> italic <|> asterisk <|>
    code <|> backtick <|>
    escape <|>
    githubLink <|> link <|> leftBracket <|>
    tag <|> lessThan <|>
    normal
  where
    bold = try $ H.b <$> (string "**" *> phrase <* string "**")
    italic = try $ H.i <$> (char '*' *> phrase <* char '*')
    asterisk = toHtml <$> takeWhile1 (== '*')

    boldU = try $ H.b <$> (string "__" *> phrase <* string "__")
    italicU = try $ H.i <$> (char '_' *> phrase <* char '_')
    underscore = toHtml <$> takeWhile1 (== '_')

    code = try $ H.code <$> (char '`' *> phrase <* char '`')
    backtick = toHtml <$> takeWhile1 (== '`')

    escape = char '\\' *>
        ((toHtml <$> satisfy (inClass "`*_\\")) <|>
         return "\\")

    normal = toHtml <$> takeWhile1 (notInClass "*_`\\[<")

    githubLink = try $ do
        _ <- string "[["
        t1 <- takeWhile1 (\c -> c /= '|' && c /= ']')
        mt2 <- (char '|' >> fmap Just (takeWhile1 (/= ']'))) <|>
               return Nothing
        _ <- string "]]"

        let (href', text) =
                case mt2 of
                    Nothing -> (t1, t1)
                    Just t2 -> (t2, t1)
        let href = T.map fix href'
            fix ' ' = '-'
            fix '/' = '-'
            fix c   = c
        return $ H.a ! HA.href (toValue href) $ toHtml text

    link = try $ do
        _ <- char '['
        t <- toHtml <$> takeWhile (/= ']')
        _ <- char ']'
        _ <- char '('
        h <- toValue <$> many1 hrefChar
        mtitle <- optional linkTitle
        _ <- char ')'
        return $ case mtitle of
            Nothing -> H.a ! HA.href h $ t
            Just title -> H.a ! HA.href h ! HA.title (toValue title) $ toHtml t
    leftBracket = toHtml <$> takeWhile1 (== '[')

    tag = try $ do
        _ <- char '<'
        name <- takeWhile1 $ \c -> not (isSpace c) && c /= '>'
        guard $ T.all (\c -> isAlpha c || c == '/') name
        rest <- takeWhile (/= '>')
        _ <- char '>'
        return $ preEscapedText $ T.concat ["<", name, rest, ">"]

    lessThan = char '<' >> return "<"

hrefChar :: Parser Char
hrefChar = (char '\\' *> anyChar) <|> satisfy (notInClass " )")

linkTitle :: Parser String
linkTitle = string " \"" *> many titleChar <* char '"'

titleChar :: Parser Char
titleChar = (char '\\' *> anyChar) <|> satisfy (/= '"')

char' :: Char -> Parser ()
char' c = char c *> return ()
