{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown
    ( MarkdownSettings
    , msXssProtect
    , def
    , markdown
    ) where

import Prelude hiding (sequence, takeWhile)
import Data.Default (Default (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze (Html, toHtml, preEscapedText)
import Data.Enumerator
    ( Iteratee, Enumeratee
    , ($$), (=$)
    , run_, enumList, consume
    , sequence
    )
import Data.Enumerator.List (fold)
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Data.Functor.Identity (runIdentity)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Attoparsec.Text
    ( Parser, takeWhile, string, skip, char, parseOnly, try
    , takeWhile1, notInClass, inClass, satisfy
    )
import Control.Applicative ((<$>), (<|>), optional, (*>), (<*), many)
import qualified Text.Blaze.Html5 as H
import Control.Monad (when)
import Text.HTML.SanitizeXSS (sanitizeBalance)

data MarkdownSettings = MarkdownSettings
    { msXssProtect :: Bool
    }

instance Default MarkdownSettings where
    def = MarkdownSettings
        { msXssProtect = True
        }

markdown :: MarkdownSettings -> TL.Text -> Html
markdown ms tl =
    runIdentity $ run_ $ enumList 8 (TL.toChunks tl) $$ markdownIter ms

markdownIter :: Monad m
             => MarkdownSettings
             -> Iteratee Text m Html
markdownIter ms = markdownEnum ms =$ fold mappend mempty

markdownEnum :: Monad m
             => MarkdownSettings
             -> Enumeratee Text Html m a
markdownEnum = sequence . iterParser . parser

nonEmptyLines :: Parser [Html]
nonEmptyLines = map line <$> nonEmptyLinesText

nonEmptyLinesText :: Parser [Text]
nonEmptyLinesText =
    go id
  where
    go :: ([Text] -> [Text]) -> Parser [Text]
    go front = do
        l <- takeWhile (/= '\n')
        optional $ skip (== '\n')
        if T.null l then return (front []) else go $ front . (l:)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

parser :: MarkdownSettings -> Parser Html
parser ms =
    html
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

    para = do
        ls <- nonEmptyLines
        when (null ls) $ fail "Missing lines"
        return $ H.p $ foldr1
            (\a b -> a <> preEscapedText "\n" <> b) ls

line :: Text -> Html
line = either error mconcat . parseOnly (many phrase)

phrase :: Parser Html
phrase =
    boldU <|> italicU <|> underscore <|>
    bold <|> italic <|> asterisk <|>
    code <|> backtick <|>
    escape <|>
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

    normal = toHtml <$> takeWhile1 (notInClass "*_`\\")
