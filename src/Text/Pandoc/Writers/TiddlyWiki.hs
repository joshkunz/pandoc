module Text.Pandoc.Writers.TiddlyWiki ( writeTiddlyWiki ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Definition (Pandoc)

-- | Convert Pandoc to TiddlyWiki.
writeTiddlyWiki:: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTiddlyWiki _ _ =
    return $ T.pack "Hello Pandoc"
