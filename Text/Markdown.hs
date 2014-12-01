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
    , msBlockCodeRenderer
    , msLinkNewTab
    , msBlankBeforeBlockquote
    , msBlockFilter
    , msAddHeadingId
      -- * Newtype
    , Markdown (..)
      -- * Fenced handlers
    , FencedHandler (..)
    , codeFencedHandler
    , htmlFencedHandler
      -- * Convenience re-exports
    , def
    ) where

import Control.Arrow ((&&&))
import Text.Markdown.Inline
import Text.Markdown.Block
import Text.Markdown.Types
import Prelude hiding (sequence, takeWhile)
import Data.Char (isAlphaNum)
import Data.Default (Default (..))
import Data.List (intercalate, isInfixOf)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.Blaze (toValue)
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
  deriving(Eq, Ord, Monoid, IsString, Show)

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
    blocksH :: [Block Html]
    blocksH = processBlocks blocks

    blocks :: [Block Text]
    blocks = runIdentity
           $ CL.sourceList (TL.toChunks tl)
          $$ toBlocks ms
          =$ CL.consume

    processBlocks :: [Block Text] -> [Block Html]
    processBlocks = map (fmap $ toHtmlI ms)
                  . msBlockFilter ms
                  . map (fmap $ intercalate [InlineHtml "<br>"])
                  . map (fmap $ map $ toInline refs)
                  . map toBlockLines

    refs =
        Map.unions $ map toRef blocks
      where
        toRef (BlockReference x y) = Map.singleton x y
        toRef _ = Map.empty

data MState = NoState | InList ListType

toHtmlB :: Monad m => MarkdownSettings -> Conduit (Block Html) m Html
toHtmlB ms =
    loop NoState
  where
    loop state = await >>= maybe
        (closeState state)
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
    go (BlockCode a b) = msBlockCodeRenderer ms a (id &&& toMarkup $ b)
    go (BlockQuote bs) = H.blockquote $ blocksToHtml bs
    go BlockRule = H.hr
    go (BlockHeading level h)
        | msAddHeadingId ms = wrap level H.! HA.id (clean h) $ h
        | otherwise         = wrap level h
      where
       wrap 1 = H.h1
       wrap 2 = H.h2
       wrap 3 = H.h3
       wrap 4 = H.h4
       wrap 5 = H.h5
       wrap _ = H.h6

       isValidChar c = isAlphaNum c || isInfixOf [c] "-_:."

       clean = toValue . TL.filter isValidChar . (TL.replace " " "-") . TL.toLower . renderHtml



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
    go (InlineLink url Nothing content)
        | msLinkNewTab ms = H.a H.! HA.href (H.toValue url) H.! HA.target "_blank" $ gos content
        | otherwise = H.a H.! HA.href (H.toValue url) $ gos content
    go (InlineLink url (Just title) content)
        | msLinkNewTab ms = H.a H.! HA.href (H.toValue url) H.! HA.title (H.toValue title) H.! HA.target "_blank" $ gos content
        | otherwise = H.a H.! HA.href (H.toValue url) H.! HA.title (H.toValue title) $ gos content
    go (InlineImage url Nothing content) = H.img H.! HA.src (H.toValue url) H.! HA.alt (H.toValue content)
    go (InlineImage url (Just title) content) = H.img H.! HA.src (H.toValue url) H.! HA.alt (H.toValue content) H.! HA.title (H.toValue title)
    go (InlineHtml t) = escape t
    go (InlineFootnoteRef x) = let ishown = TL.pack (show x)
                                   (<>) = mappend
                                in H.a H.! HA.href (H.toValue $ "#footnote-" <> ishown)
                                       H.! HA.id (H.toValue $ "ref-" <> ishown) $ H.toHtml $ "[" <> ishown <> "]"
    go (InlineFootnote x) = let ishown = TL.pack (show x)
                                (<>) = mappend
                             in H.a H.! HA.href (H.toValue $ "#ref-" <> ishown)
                                    H.! HA.id (H.toValue $ "footnote-" <> ishown) $ H.toHtml $ "[" <> ishown <> "]"
