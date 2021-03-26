module Text.Pandoc.Writers.TiddlyWiki ( writeTiddlyWiki ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Definition

-- | Convert Pandoc to TiddlyWiki.
writeTiddlyWiki:: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTiddlyWiki _ (Pandoc _ blocks) =
    return . mconcat . map (endBlock . writeBlock) $ blocks
    where endBlock t = if T.null t then t else t <> (T.pack "\n")

writeBlock :: Block -> Text
writeBlock Null = T.empty

-- TODO(jkz): Handle all cases.
writeBlock _ = T.empty
