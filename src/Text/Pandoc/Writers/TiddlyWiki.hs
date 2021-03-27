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

writeBlocks :: PandocMonad m => [Block] -> m Text
writeBlocks blocks =
    (fmap mconcat) . mapM ((fmap endBlock) . writeBlock) $ blocks
    where endBlock t = if T.null t then t else t <> "\n\n"

writeBlock :: PandocMonad m => Block -> m Text

writeBlock Null = return T.empty

-- https://tiddlywiki.com/#Horizontal%20Rules%20in%20WikiText
writeBlock HorizontalRule = return "---"

-- https://tiddlywiki.com/#HTML%20in%20WikiText
writeBlock (RawBlock f text)
    | f == Format "html" = return $ text <> "\n"
    | f == Format "tiddlywiki" = return $ text <> "\n"
writeBlock b@(RawBlock _ _) = T.empty <$ report (BlockNotRendered b)

-- TODO(jkz): Handle all cases.
writeBlock _ = return T.empty
