{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.Markdown.Block
    ( Block (..)
    , ListType (..)
    , toBlocks
    ) where

import Prelude
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
    | BlockRule
    | BlockHeading Int inline
    | BlockReference Text Text
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

toBlocks :: Monad m => Conduit Text m (Block Text)
toBlocks =
    mapOutput fixWS CT.lines =$= toBlocksLines
  where
    fixWS = T.pack . go 0 . T.unpack

    go _ [] = []
    go i ('\r':cs) = go i cs
    go i ('\t':cs) =
        (replicate j ' ') ++ go (i + j) cs
      where
        j = 4 - (i `mod` 4)
    go i (c:cs) = c : go (i + 1) cs

toBlocksLines :: Monad m => Conduit Text m (Block Text)
toBlocksLines = awaitForever start =$= tightenLists

tightenLists :: Monad m => GLInfConduit (Either Blank (Block Text)) m (Block Text)
tightenLists =
    go Nothing
  where
    go mTightList =
        awaitE >>= either return go'
      where
        go' (Left Blank) = go mTightList
        go' (Right (BlockList ltNew contents)) =
            case mTightList of
                Just (ltOld, isTight) | ltOld == ltNew -> do
                    yield $ BlockList ltNew $ (if isTight then tighten else untighten) contents
                    go mTightList
                _ -> do
                    isTight <- checkTight ltNew False
                    yield $ BlockList ltNew $ (if isTight then tighten else untighten) contents
                    go $ Just (ltNew, isTight)
        go' (Right b) = yield b >> go Nothing

    tighten (Right [BlockPara t]) = Left t
    tighten (Right []) = Left T.empty
    tighten x = x

    untighten (Left t) = Right [BlockPara t]
    untighten x = x

    checkTight lt sawBlank = do
        await >>= maybe (return $ not sawBlank) go'
      where
        go' (Left Blank) = checkTight lt True
        go' b@(Right (BlockList ltNext _)) | ltNext == lt = do
            leftover b
            return $ not sawBlank
        go' b = leftover b >> return False

data Blank = Blank

start :: Monad m => Text -> GLConduit Text m (Either Blank (Block Text))
start t
    | T.null $ T.strip t = yield $ Left Blank
    | Just lang <- T.stripPrefix "~~~" t = do
        (finished, ls) <- takeTill (== "~~~") >+> withUpstream CL.consume
        case finished of
            Just _ -> yield $ Right $ BlockCode (if T.null lang then Nothing else Just lang) $ T.intercalate "\n" ls
            Nothing -> mapM_ leftover (reverse $ T.cons ' ' t : ls)
    | Just lang <- T.stripPrefix "```" t = do
        (finished, ls) <- takeTill (== "```") >+> withUpstream CL.consume
        case finished of
            Just _ -> yield $ Right $ BlockCode (if T.null lang then Nothing else Just lang) $ T.intercalate "\n" ls
            Nothing -> mapM_ leftover (reverse $ T.cons ' ' t : ls)
    | Just t' <- T.stripPrefix "> " t = do
        ls <- takeQuotes >+> CL.consume
        let blocks = runIdentity $ mapM_ yield (t' : ls) $$ toBlocksLines =$ CL.consume
        yield $ Right $ BlockQuote blocks
    | Just (level, t') <- stripHeading t = yield $ Right $ BlockHeading level t'
    | Just t' <- T.stripPrefix "    " t = do
        ls <- getIndented 4 >+> CL.consume
        yield $ Right $ BlockCode Nothing $ T.intercalate "\n" $ t' : ls
    | isRule t = yield $ Right BlockRule
    | isHtmlStart t = do
        ls <- takeTill (T.null . T.strip) >+> CL.consume
        yield $ Right $ BlockHtml $ T.intercalate "\n" $ t : ls
    | Just (ltype, t') <- listStart t = do
        let t'' = T.dropWhile (== ' ') t'
        let leader = T.length t - T.length t''
        ls <- getIndented leader >+> CL.consume
        let blocks = runIdentity $ mapM_ yield (t'' : ls) $$ toBlocksLines =$ CL.consume
        yield $ Right $ BlockList ltype $ Right blocks

    | Just (x, y) <- getReference t = yield $ Right $ BlockReference x y

    | otherwise = do
        -- Check for underline headings
        t2 <- CL.peek
        case t2 >>= getUnderline of
            Nothing -> do
                let listStartIndent x =
                        case listStart x of
                            Just (_, y) -> T.take 2 y == "  "
                            Nothing -> False
                (mfinal, ls) <- takeTill (\x -> T.null (T.strip x) || listStartIndent x) >+> withUpstream CL.consume
                maybe (return ()) leftover mfinal
                yield $ Right $ BlockPara $ T.intercalate "\n" $ t : ls
            Just level -> do
                CL.drop 1
                yield $ Right $ BlockHeading level t

isHtmlStart :: T.Text -> Bool
isHtmlStart t =
    case T.stripPrefix "<" t of
        Nothing -> False
        Just t' ->
            let (name, rest) = T.break (\c -> c `elem` " >/") t'
             in T.all isValidTagName name &&
                not (T.null name) &&
                (not ("/" `T.isPrefixOf` rest) || ("/>" `T.isPrefixOf` rest))
  where
    isValidTagName :: Char -> Bool
    isValidTagName c =
        ('A' <= c && c <= 'Z') ||
        ('a' <= c && c <= 'z') ||
        ('0' <= c && c <= '9') ||
        (c == '-') ||
        (c == '_') ||
        (c == '!')

takeTill :: Monad m => (i -> Bool) -> Pipe l i i u m (Maybe i)
takeTill f =
    loop
  where
    loop = await >>= maybe (return Nothing) (\x -> if f x then return (Just x) else yield x >> loop)

listStart :: Text -> Maybe (ListType, Text)
listStart t0
    | Just t' <- T.stripPrefix "* " t = Just (Unordered, t')
    | Just t' <- T.stripPrefix "+ " t = Just (Unordered, t')
    | Just t' <- T.stripPrefix "- " t = Just (Unordered, t')
    | Just t' <- stripNumber t, Just t'' <- stripSeparator t' = Just (Ordered, t'')
    | otherwise = Nothing
  where
    t = T.stripStart t0

stripNumber :: Text -> Maybe Text
stripNumber x
    | T.null y = Nothing
    | otherwise = Just z
  where
    (y, z) = T.span isDigit x

stripSeparator :: Text -> Maybe Text
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
        | otherwise = mapM_ leftover (t:blanks)
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

isRule :: Text -> Bool
isRule =
    go . T.strip
  where
    go "* * *" = True
    go "***" = True
    go "*****" = True
    go "- - -" = True
    go "---" = True
    go "___" = True
    go "_ _ _" = True
    go t = T.length (T.takeWhile (== '-') t) >= 5

stripHeading :: Text -> Maybe (Int, Text)
stripHeading t
    | T.null x = Nothing
    | otherwise = Just (T.length x, T.strip $ T.dropWhileEnd (== '#') y)
  where
    (x, y) = T.span (== '#') t

getUnderline :: Text -> Maybe Int
getUnderline t
    | T.length t < 2 = Nothing
    | T.all (== '=') t = Just 1
    | T.all (== '-') t = Just 2
    | otherwise = Nothing

getReference :: Text -> Maybe (Text, Text)
getReference a = do
    b <- T.stripPrefix "[" $ T.dropWhile (== ' ') a
    let (name, c) = T.break (== ']') b
    d <- T.stripPrefix "]:" c
    Just (name, T.strip d)
