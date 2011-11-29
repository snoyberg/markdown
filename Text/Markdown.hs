module Text.Markdown
    ( MarkdownSettings
    , msXssProtect
    , def
    , markdown
    ) where

import Prelude hiding (sequence, takeWhile)
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze (Html, toHtml)
import Data.Enumerator
    ( Iteratee, Enumeratee
    , ($$), (=$)
    , run_, enumList, consume
    , sequence
    )
import Data.Enumerator.List (fold)
import Data.Monoid (mappend, mempty)
import Data.Functor.Identity (runIdentity)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Attoparsec.Text (Parser, takeWhile)
import Control.Applicative ((<$>))
import qualified Text.Blaze.Html5 as H

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
             -> Iteratee T.Text m Html
markdownIter ms = markdownEnum ms =$ fold mappend mempty

markdownEnum :: Monad m
             => MarkdownSettings
             -> Enumeratee T.Text Html m a
markdownEnum = sequence . iterParser . parser

parser :: MarkdownSettings -> Parser Html
parser ms =
    para
  where
    para = H.p . toHtml <$> takeWhile (const True)
