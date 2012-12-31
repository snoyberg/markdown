{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Default (Default (def))
import Data.Set (Set, empty)
import Data.Map (Map, singleton)
import Data.Monoid (mappend)

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
    }

data FencedHandler = FHRaw (Text -> Block Text)
                   | FHParsed ([Block Text] -> [Block Text])

instance Default MarkdownSettings where
    def = MarkdownSettings
        { msXssProtect = True
        , msStandaloneHtml = empty
        , msFencedHandlers = codeFencedHandler "```" `mappend` codeFencedHandler "~~~"
        }

codeFencedHandler :: Text -> Map Text (Text -> FencedHandler)
codeFencedHandler key = singleton key $ \lang -> FHRaw $
    BlockCode $ if T.null lang then Nothing else Just lang

htmlFencedHandler :: Text -- ^ fence string
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
