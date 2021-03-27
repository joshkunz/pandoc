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

isBlockQuote :: Block -> Bool
isBlockQuote (BlockQuote _) = True
isBlockQuote _ = False

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

-- https://tiddlywiki.com/#Hard%20Linebreaks%20in%20WikiText
writeBlock (LineBlock iss) =
    fmap (surround "\n\"\"\"\n") . body $ iss
    where body = fmap (mconcat . (intersperse "\n")) . mapM writeInlines

-- TODO(jkz): Actually handle attrs.
writeBlock (CodeBlock _ text) = return . surround "\n```\n" $ text

writeBlock b@(BlockQuote bs)
    -- TODO(jkz): Support nested block quotes.
    | any isBlockQuote bs = T.empty <$ report (BlockNotRendered b)
    | otherwise           =
        wrapQuote <$> writeBlocks bs
        where wrapQuote t = "\n>>>\n" <> t <> "<<<\n"

-- TODO(jkz): Handle all cases.
writeBlock b = T.empty <$ report (BlockNotRendered b)

surround :: Text -> Text -> Text
surround with on = with <> on <> with

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