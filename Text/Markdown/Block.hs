{-# LANGUAGE OverloadedStrings #-}
module Text.Markdown.Block
    ( Block (..)
    , ListType (..)
    , toBlocks
    ) where

import Data.Conduit
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity (runIdentity)
import Data.Char (isDigit)

data ListType = Ordered | Unordered
  deriving (Show, Eq)

data Block inline
    = BlockPara inline
    | BlockList ListType (Either inline [Block inline])
    | BlockCode (Maybe Text) Text
    | BlockQuote [Block inline]
    | BlockHtml Text
  deriving (Show, Eq)

toBlocks :: Monad m => GInfConduit Text m (Block Text)
toBlocks = mapOutput noCR CT.lines >+> toBlocksLines

toBlocksLines :: Monad m => GInfConduit Text m (Block Text)
toBlocksLines = injectLeftovers $ awaitForever start

noCR :: Text -> Text
noCR t
    | T.null t = t
    | T.last t == '\r' = T.init t
    | otherwise = t

start :: Monad m => Text -> GLConduit Text m (Block Text)
start t
    | T.null $ T.strip t = return ()
    | Just lang <- T.stripPrefix "~~~" t = do
        (finished, ls) <- takeTill (== "~~~") >+> withUpstream CL.consume
        if finished
            then yield $ BlockCode (if T.null lang then Nothing else Just lang) $ T.intercalate "\n" ls
            else mapM_ leftover (reverse $ T.cons ' ' t : ls)
    | Just t' <- T.stripPrefix "> " t = do
        ls <- takeQuotes >+> CL.consume
        let blocks = runIdentity $ mapM_ yield (t' : ls) $$ toBlocksLines =$ CL.consume
        yield $ BlockQuote blocks
    | Just t' <- T.stripPrefix "    " t = do
        ls <- getIndented 4 >+> CL.consume
        yield $ BlockCode Nothing $ T.intercalate "\n" $ t' : ls
    | T.isPrefixOf "<" t = do
        ls <- takeTill (T.null . T.strip) >+> CL.consume
        yield $ BlockHtml $ T.intercalate "\n" $ t : ls
    | Just (ltype, t') <- listStart t = do
        let (spaces, t'') = T.span (== ' ') t'
        if T.length spaces >= 2
            then do
                let leader = T.length t - T.length t''
                ls <- getIndented leader >+> CL.consume
                let blocks = runIdentity $ mapM_ yield (t'' : ls) $$ toBlocksLines =$ CL.consume
                yield $ BlockList ltype $ Right blocks
            else yield $ BlockList ltype $ Left t''

    | otherwise = do
        ls <- takeTill (T.null . T.strip) >+> CL.consume
        yield $ BlockPara $ T.unwords $ t : ls

takeTill f =
    loop
  where
    loop = await >>= maybe (return False) (\x -> if f x then return True else yield x >> loop)

listStart t
    | Just t' <- T.stripPrefix "* " t = Just (Unordered, t')
    | Just t' <- stripNumber t, Just t'' <- stripSeparator t' = Just (Ordered, t'')
    | otherwise = Nothing

stripNumber x
    | T.null y = Nothing
    | otherwise = Just z
  where
    (y, z) = T.span isDigit x

stripSeparator x =
    case T.uncons x of
        Nothing -> Nothing
        Just ('.', y) -> Just y
        Just (')', y) -> Just y
        _ -> Nothing

getIndented :: Monad m => Int -> GLConduit Text m Text
getIndented leader =
    go []
  where
    go blanks = await >>= maybe (mapM_ leftover blanks) (go' blanks)

    go' blanks t
        | T.null $ T.strip t = go (T.drop leader t : blanks)
        | T.length x == leader && T.null (T.strip x) = do
            mapM_ yield $ reverse blanks
            yield y
            go []
        | otherwise = mapM_ leftover blanks >> leftover t
      where
        (x, y) = T.splitAt leader t

takeQuotes :: Monad m => GLConduit Text m Text
takeQuotes =
    await >>= maybe (return ()) go
  where
    go ">" = yield "" >> takeQuotes
    go t
        | Just t' <- T.stripPrefix "> " t = yield t' >> takeQuotes
        | otherwise = leftover t
