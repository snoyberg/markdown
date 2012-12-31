{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Text.Markdown
    ( -- * Functions
      markdown
      -- * Settings
    , MarkdownSettings
    , msXssProtect
    , msStandaloneHtml
    , msFencedHandlers
      -- * Newtype
    , Markdown (..)
      -- * Fenced handlers
    , FencedHandler (..)
    , codeFencedHandler
    , htmlFencedHandler
      -- * Convenience re-exports
    , def
    ) where

import Text.Markdown.Inline
import Text.Markdown.Block
import Text.Markdown.Types
import Prelude hiding (sequence, takeWhile)
import Data.Default (Default (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html (ToMarkup (..), Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Data.Functor.Identity (runIdentity)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.HTML.SanitizeXSS (sanitizeBalance)
import qualified Data.Map as Map
import Data.String (IsString)

-- | A newtype wrapper providing a @ToHtml@ instance.
newtype Markdown = Markdown TL.Text
  deriving(Monoid, IsString)

instance ToMarkup Markdown where
    toMarkup (Markdown t) = markdown def t

-- | Convert the given textual markdown content to HTML.
--
-- >>> :set -XOverloadedStrings
-- >>> import Text.Blaze.Html.Renderer.Text
-- >>> renderHtml $ markdown def "# Hello World!"
-- "<h1>Hello World!</h1>"
--
-- >>> renderHtml $ markdown def { msXssProtect = False } "<script>alert('evil')</script>"
-- "<script>alert('evil')</script>"
markdown :: MarkdownSettings -> TL.Text -> Html
markdown ms tl =
       sanitize
     $ runIdentity
     $ CL.sourceList blocksH
    $= toHtmlB ms
    $$ CL.fold mappend mempty
  where
    sanitize
        | msXssProtect ms = preEscapedToMarkup . sanitizeBalance . TL.toStrict . renderHtml
        | otherwise = id
    fixBlock :: Block Text -> Block Html
    fixBlock = fmap $ toHtmlI ms . toInline refs

    blocksH :: [Block Html]
    blocksH = map fixBlock blocks

    blocks :: [Block Text]
    blocks = runIdentity
           $ CL.sourceList (TL.toChunks tl)
          $$ toBlocks ms
          =$ CL.consume

    refs =
        Map.unions $ map toRef blocks
      where
        toRef (BlockReference x y) = Map.singleton x y
        toRef _ = Map.empty

data MState = NoState | InList ListType

toHtmlB :: Monad m => MarkdownSettings -> GInfConduit (Block Html) m Html
toHtmlB ms =
    loop NoState
  where
    loop state = awaitE >>= either
        (\e -> closeState state >> return e)
        (\x -> do
            state' <- getState state x
            yield $ go x
            loop state')

    closeState NoState = return ()
    closeState (InList Unordered) = yield $ escape "</ul>"
    closeState (InList Ordered) = yield $ escape "</ol>"

    getState NoState (BlockList ltype _) = do
        yield $ escape $
            case ltype of
                Unordered -> "<ul>"
                Ordered -> "<ol>"
        return $ InList ltype
    getState NoState _ = return NoState
    getState state@(InList lt1) b@(BlockList lt2 _)
        | lt1 == lt2 = return state
        | otherwise = closeState state >> getState NoState b
    getState state@(InList _) _ = closeState state >> return NoState

    go (BlockPara h) = H.p h
    go (BlockPlainText h) = h
    go (BlockList _ (Left h)) = H.li h
    go (BlockList _ (Right bs)) = H.li $ blocksToHtml bs
    go (BlockHtml t) = escape t
    go (BlockCode Nothing t) = H.pre $ H.code $ toMarkup t
    go (BlockCode (Just lang) t) = H.pre $ H.code H.! HA.class_ (H.toValue lang) $ toMarkup t
    go (BlockQuote bs) = H.blockquote $ blocksToHtml bs
    go BlockRule = H.hr
    go (BlockHeading level h) =
        wrap level h
      where
       wrap 1 = H.h1
       wrap 2 = H.h2
       wrap 3 = H.h3
       wrap 4 = H.h4
       wrap 5 = H.h5
       wrap _ = H.h6
    go BlockReference{} = return ()

    blocksToHtml bs = runIdentity $ mapM_ yield bs $$ toHtmlB ms =$ CL.fold mappend mempty

escape :: Text -> Html
escape = preEscapedToMarkup

toHtmlI :: MarkdownSettings -> [Inline] -> Html
toHtmlI ms is0
    | msXssProtect ms = escape $ sanitizeBalance $ TL.toStrict $ renderHtml final
    | otherwise = final
  where
    final = gos is0
    gos = mconcat . map go

    go (InlineText t) = toMarkup t
    go (InlineItalic is) = H.i $ gos is
    go (InlineBold is) = H.b $ gos is
    go (InlineCode t) = H.code $ toMarkup t
    go (InlineLink url Nothing content) = H.a H.! HA.href (H.toValue url) $ gos content
    go (InlineLink url (Just title) content) = H.a H.! HA.href (H.toValue url) H.! HA.title (H.toValue title) $ gos content
    go (InlineImage url Nothing content) = H.img H.! HA.src (H.toValue url) H.! HA.alt (H.toValue content)
    go (InlineImage url (Just title) content) = H.img H.! HA.src (H.toValue url) H.! HA.alt (H.toValue content) H.! HA.title (H.toValue title)
    go (InlineHtml t) = escape t
