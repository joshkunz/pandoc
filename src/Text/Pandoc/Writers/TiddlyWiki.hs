{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.TiddlyWiki ( writeTiddlyWiki ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Definition
import Text.Pandoc.Logging (LogMessage(..))

-- | Convert Pandoc to TiddlyWiki.
writeTiddlyWiki:: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTiddlyWiki _ (Pandoc _ blocks) = writeBlocks blocks

repeatString :: Int -> String -> String
repeatString n = mconcat . replicate n

writeBlocks :: PandocMonad m => [Block] -> m Text
writeBlocks blocks = (fmap mconcat) . mapM writeBlock $ blocks

writeBlock :: PandocMonad m => Block -> m Text

writeBlock Null = return T.empty

-- https://tiddlywiki.com/#Horizontal%20Rules%20in%20WikiText
writeBlock HorizontalRule = return "---\n"

-- https://tiddlywiki.com/#HTML%20in%20WikiText
writeBlock (RawBlock f text)
    | f == Format "html" = return $ text <> "\n"
    | f == Format "tiddlywiki" = return $ text <> "\n"
writeBlock b@(RawBlock _ _) = T.empty <$ report (BlockNotRendered b)

writeBlock (Plain inlines) = writeInlines inlines
writeBlock (Para inlines) = (<> "\n\n") <$> writeInlines inlines

-- https://tiddlywiki.com/#Headings%20in%20WikiText
-- TODO(jkz): Support Attrs
writeBlock (Header level _ inlines) =
    header <$> writeInlines inlines
    where header v = mconcat ["\n", T.pack (repeatString level "!"), " ", v, "\n"]

-- TODO(jkz): Handle all cases.
writeBlock b = T.empty <$ report (BlockNotRendered b)

writeInlines :: PandocMonad m => [Inline] -> m Text
writeInlines inlines =
    (fmap mconcat) . mapM writeInline $ inlines

writeInline :: PandocMonad m => Inline -> m Text

writeInline Space = return " "
writeInline SoftBreak = return "\n"

writeInline (Str t) = return t

-- TODO(jkz): Handle all inlines.
writeInline i = T.empty <$ report (InlineNotRendered i)