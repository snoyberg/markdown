{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Default (Default (def))
import Data.Set (Set, empty)
import Data.Map (Map, singleton)
import Data.Monoid (mappend)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

-- | A settings type providing various configuration options.
--
-- See <http://www.yesodweb.com/book/settings-types> for more information on
-- settings types. In general, you can use @def@.
data MarkdownSettings = MarkdownSettings
    { msXssProtect :: Bool
      -- ^ Whether to automatically apply XSS protection to embedded HTML. Default: @True@.
    , msStandaloneHtml :: Set Text
      -- ^ HTML snippets which stand on their own. We do not require a blank line following these pieces of HTML.
      --
      -- Default: empty set.
      --
      -- Since: 0.1.2
    , msFencedHandlers :: Map Text (Text -> FencedHandler)
      -- ^ Handlers for the special \"fenced\" format. This is most commonly
      -- used for fenced code, e.g.:
      --
      -- > ```haskell
      -- > main = putStrLn "Hello"
      -- > ```
      --
      -- This is an extension of Markdown, but a fairly commonly used one.
      --
      -- This setting allows you to create new kinds of fencing. Fencing goes
      -- into two categories: parsed and raw. Code fencing would be in the raw
      -- category, where the contents are not treated as Markdown. Parsed will
      -- treat the contents as Markdown and allow you to perform some kind of
      -- modifcation to it.
      --
      -- For example, to create a new @\@\@\@@ fencing which wraps up the
      -- contents in an @article@ tag, you could use:
      --
      -- > def { msFencedHandlers = htmlFencedHandler "@@@" (const "<article>") (const "</article")
      -- >              `Map.union` msFencedHandlers def
      -- >     }
      --
      -- Default: code fencing for @```@ and @~~~@.
      --
      -- Since: 0.1.2
    , msBlockCodeRenderer :: Maybe Text -> (Text,Html) -> Html
      -- ^ A rendering function through which code blocks are passed.
      --
      -- The arguments are the block's language, if any, and the tuple
      -- @(unrendered content, rendered content)@. For example, if you wanted to pass
      -- code blocks in your markdown text through a highlighter like @highlighting-kate@,
      -- you might do something like:
      --
      -- >>> :set -XOverloadedStrings
      -- >>> let renderer lang (src,_) = formatHtmlBlock defaultFormatOpts $ highlightAs (maybe "text" unpack lang) $ unpack src
      -- >>> let md = markdown def { msBlockCodeRenderer = renderer } "``` haskell\nmain = putStrLn \"Hello world!\"\n```"
      -- >>> putStrLn $ renderHtml md
      -- <pre class="sourceCode"><code class="sourceCode">main <span class="fu">=</span> <span class="fu">putStrLn</span> <span class="st">&quot;Hello world!&quot;</span></code></pre>
      --
      -- Since: 0.1.2.1
    , msLinkNewTab :: Bool
      -- ^ If @True@, all generated links have the attribute target=_blank set,
      -- causing them to be opened in a new tab or window.
      --
      -- Default: @False@
      --
      -- Since 0.1.4

    , msBlankBeforeBlockquote :: Bool
      -- ^ If @True@, a blank line is required before the start of a blockquote.  Standard
      -- markdown syntax does not require a blank line before a blockquote, but it is all
      -- too easy for a > to end up at the beginning of a line by accident.
      --
      -- Default: @True@
      --
      -- Since 0.1.5
    , msBlockFilter :: [Block [Inline]] -> [Block [Inline]]
      -- ^ A function to filter and/or modify parsed blocks before they are
      -- written to Html
      --
      -- Default: @id@
      --
      -- Since 0.1.7

    , msAddHeadingId :: Bool
      -- ^ If @True@, an @id@ attribute is added to the heading tag with the value equal to
      -- the text with only valid CSS identifier characters.
      --
      -- > ## Executive Summary
      --
      -- > <h2 id="executive-summary">Executive Summary</h2>
      --
      -- Default: @False@
      --
      -- Since 0.1.13

    }

-- | See 'msFencedHandlers.
--
-- Since 0.1.2
data FencedHandler = FHRaw (Text -> [Block Text])
                     -- ^ Wrap up the given raw content.
                   | FHParsed ([Block Text] -> [Block Text])
                     -- ^ Wrap up the given parsed content.

instance Default MarkdownSettings where
    def = MarkdownSettings
        { msXssProtect = True
        , msStandaloneHtml = empty
        , msFencedHandlers = codeFencedHandler "```" `mappend` codeFencedHandler "~~~"
        , msBlockCodeRenderer =
            \lang (_,rendered) -> case lang of
                                       Just l -> H.pre $ H.code H.! HA.class_ (H.toValue l) $ rendered
                                       Nothing -> H.pre $ H.code $ rendered
        , msLinkNewTab = False
        , msBlankBeforeBlockquote = True
        , msBlockFilter = id
        , msAddHeadingId = False
        }

-- | Helper for creating a 'FHRaw'.
--
-- Since 0.1.2
codeFencedHandler :: Text -- ^ Delimiter
                  -> Map Text (Text -> FencedHandler)
codeFencedHandler key = singleton key $ \lang -> FHRaw $
    return . BlockCode (if T.null lang then Nothing else Just lang)

-- | Helper for creating a 'FHParsed'.
--
-- Note that the start and end parameters take a @Text@ parameter; this is the
-- text following the delimiter. For example, with the markdown:
--
-- > @@@ foo
--
-- @foo@ would be passed to start and end.
--
-- Since 0.1.2
htmlFencedHandler :: Text -- ^ Delimiter
                  -> (Text -> Text) -- ^ start HTML
                  -> (Text -> Text) -- ^ end HTML
                  -> Map Text (Text -> FencedHandler)
htmlFencedHandler key start end = singleton key $ \lang -> FHParsed $ \blocks ->
      BlockHtml (start lang)
    : blocks
   ++ [BlockHtml $ end lang]

data ListType = Ordered | Unordered
  deriving (Show, Eq)

data Block inline
    = BlockPara inline
    | BlockList ListType (Either inline [Block inline])
    | BlockCode (Maybe Text) Text
    | BlockQuote [Block inline]
    | BlockHtml Text
    | BlockRule
    | BlockHeading Int inline
    | BlockReference Text Text
    | BlockPlainText inline
  deriving (Show, Eq)

instance Functor Block where
    fmap f (BlockPara i) = BlockPara (f i)
    fmap f (BlockList lt (Left i)) = BlockList lt $ Left $ f i
    fmap f (BlockList lt (Right bs)) = BlockList lt $ Right $ map (fmap f) bs
    fmap _ (BlockCode a b) = BlockCode a b
    fmap f (BlockQuote bs) = BlockQuote $ map (fmap f) bs
    fmap _ (BlockHtml t) = BlockHtml t
    fmap _ BlockRule = BlockRule
    fmap f (BlockHeading level i) = BlockHeading level (f i)
    fmap _ (BlockReference x y) = BlockReference x y
    fmap f (BlockPlainText x) = BlockPlainText (f x)

data Inline = InlineText Text
            | InlineItalic [Inline]
            | InlineBold [Inline]
            | InlineCode Text
            | InlineHtml Text
            | InlineLink Text (Maybe Text) [Inline] -- ^ URL, title, content
            | InlineImage Text (Maybe Text) Text -- ^ URL, title, content
            | InlineFootnoteRef Integer -- ^ The footnote reference in the body
            | InlineFootnote Integer
    deriving (Show, Eq)
