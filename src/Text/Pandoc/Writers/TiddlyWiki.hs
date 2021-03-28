{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.TiddlyWiki ( writeTiddlyWiki ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Definition
import Text.Pandoc.Logging (LogMessage(..))
import Data.List (intersperse)

-- | Convert Pandoc to TiddlyWiki.
writeTiddlyWiki:: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTiddlyWiki _ (Pandoc _ blocks) = writeBlocks blocks

repeatString :: Int -> String -> String
repeatString n = mconcat . replicate n

-- | Wrap the given text `on` with the string `with`.
surround :: Text -> Text -> Text
surround with on = with <> on <> with

-- | Wrap the given text `on` with the string `with` with newlines
-- | before and after.
surroundBlock :: Text -> Text -> Text
surroundBlock with on = surroundBlock2 with with on

-- | Wrap the given text `on` with `begin` and `end`.
surroundBlock2 :: Text -> Text -> Text -> Text
surroundBlock2 begin end on = begin <> "\n" <> on <> "\n" <> end

isBlockQuote :: Block -> Bool
isBlockQuote (BlockQuote _) = True
isBlockQuote _ = False

isBreak :: Inline -> Bool
isBreak SoftBreak = True
isBreak LineBreak = True
isBreak _ = False

-- | Evaluates if the given block consists of a single line. Only single-line
-- | blocks (and other list blocks) are legal list elements.
isSingleLine :: Block -> Bool
isSingleLine =
    maybe False (not . any isBreak) . inlines
    where inlines (Plain is) = Just is
          inlines (Para is) = Just is
          inlines _ = Nothing

-- | Add the given prefix to the beginning of each line in the given text.
indent :: Text -> Text -> Text
indent prefix = T.unlines . map (prefix <>) . T.lines

writeBlocks :: PandocMonad m => [Block] -> m Text
writeBlocks blocks =
    joinTexts <$> mapM writeBlock blocks
    where joinTexts = mconcat . (intersperse "\n\n") . filter (not . T.null)

writeBlock :: PandocMonad m => Block -> m Text

writeBlock Null = return T.empty

-- https://tiddlywiki.com/#Horizontal%20Rules%20in%20WikiText
writeBlock HorizontalRule = return "---"

-- https://tiddlywiki.com/#HTML%20in%20WikiText
writeBlock (RawBlock f text)
    | f == Format "html" = return text
    | f == Format "tiddlywiki" = return text
writeBlock b@(RawBlock _ _) = T.empty <$ report (BlockNotRendered b)

writeBlock (Plain inlines) = writeInlines inlines
writeBlock (Para inlines) = writeInlines inlines

-- https://tiddlywiki.com/#Headings%20in%20WikiText
-- TODO(jkz): Support Attrs
writeBlock (Header level _ inlines) =
    header <$> writeInlines inlines
    where header v = T.pack (repeatString level "!") <> " " <> v

-- https://tiddlywiki.com/#Hard%20Linebreaks%20in%20WikiText
writeBlock (LineBlock iss) =
    fmap (surroundBlock "\"\"\"") . body $ iss
    where body = fmap (mconcat . (intersperse "\n")) . mapM writeInlines

-- TODO(jkz): Actually handle attrs.
writeBlock (CodeBlock _ text) = return . surroundBlock "```" $ text

writeBlock b@(BlockQuote bs)
    -- TODO(jkz): Support nested block quotes.
    | any isBlockQuote bs = T.empty <$ report (BlockNotRendered b)
    | otherwise           =
        surroundBlock2 ">>>" "<<<" <$> writeBlocks bs

writeBlock b@(BulletList bss) =
    case mapM extract . concat $ bss of
        -- TiddlyWiki only supports inline elements and other lists in bulleted
        -- lists. Fail if we're asked to add a bullet with more than one block.
        Nothing -> T.empty <$ report (BlockNotRendered b)
        Just bs -> fmap (mconcat . (intersperse (T.pack "\n"))) . mapM writeBlockWithStar $ bs
    where extract block@(BulletList _) = Just block
          extract block | isSingleLine block = Just block
          extract _ = Nothing
          writeBlockWithStar block@(BulletList _) =
              indent "*" <$> writeBlock block
          writeBlockWithStar block = ("* " <>) <$> writeBlock block

-- TODO(jkz): Handle all cases.
writeBlock b = T.empty <$ report (BlockNotRendered b)

isNormalText :: Inline -> Bool
isNormalText (Str _) = True
isNormalText Space = True
isNormalText _ = False

writeInlines :: PandocMonad m => [Inline] -> m Text
writeInlines inlines =
    (fmap mconcat) . mapM writeInline $ inlines

writeInline :: PandocMonad m => Inline -> m Text

writeInline Space = return " "
writeInline SoftBreak = return "\n"

writeInline (Str t) = return t

-- HTML is rendered direclty inline https://tiddlywiki.com/#HTML%20in%20WikiText
writeInline (RawInline f t)
    | f == Format "html" = return t
    | f == Format "tiddlywiki" = return t
writeInline i@(RawInline _ _) = T.empty <$ report (InlineNotRendered i)

writeInline (Emph is) = surround "//" <$> writeInlines is
writeInline (Underline is) = surround "__" <$> writeInlines is
writeInline (Strong is) = surround "''" <$> writeInlines is
writeInline (Strikeout is) = surround "~~" <$> writeInlines is
writeInline (Superscript is) = surround "^^" <$> writeInlines is
writeInline (Subscript is) = surround ",," <$> writeInlines is

writeInline (Code _ code)
    -- Use double-quotes if the code contains a backtick.
    | '`' `elem` (T.unpack code) = return $ surround "``" code
    | otherwise                  = return $ surround "`" code

-- TODO(jkz): Support attrs for links (since we can add them to <a>)
writeInline (Link _ is (url, _)) =
    -- Use <a> if there is non-trivial formatting in the body text.
    if all isNormalText is then wiki <$> inlineText
                           else a <$> inlineText
    where inlineText = writeInlines is
          a body = "<a href=" <> (surround "\"" url) <> ">" <> body <> "</a>"
          wiki body = "[[" <> body <> "|" <> url <> "]]"

-- TODO(jkz): Support attrs via an HTML wrapper.
writeInline (Span _ is) = writeInlines is

-- TODO(jkz): Handle all inlines.
writeInline i = T.empty <$ report (InlineNotRendered i)