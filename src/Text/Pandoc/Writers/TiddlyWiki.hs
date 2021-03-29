{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.TiddlyWiki ( writeTiddlyWiki ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Definition
import Text.Pandoc.Logging (LogMessage(..))
import Data.List (intersperse)
import Control.Monad.Reader (ReaderT(runReaderT), local, asks)
import Data.Default (Default(..))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Maybe (catMaybes)
import Data.String (IsString(..))
import Data.Char (isSpace)

data Environment =
    Environment { inBlockQuote :: Bool
                , inLineBreakBlock :: Bool
                }

enterBlockQuote :: Environment -> Environment
enterBlockQuote e = e { inBlockQuote = True }

enterLineBreakBlock :: Environment -> Environment
enterLineBreakBlock e = e { inLineBreakBlock = True }

instance Default Environment where
    def = Environment { inBlockQuote = False
                      , inLineBreakBlock = False
                      }

type TiddlyWiki m = ReaderT Environment m

runTiddlyWiki :: (PandocMonad m) => TiddlyWiki m a -> m a
runTiddlyWiki r = runReaderT r def

-- | Convert Pandoc to TiddlyWiki.
writeTiddlyWiki:: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTiddlyWiki _ (Pandoc _ blocks) =
    runTiddlyWiki $ writeBlocks blocks

mrepeated :: (Monoid m) => Int -> m -> m
mrepeated n = mconcat . replicate n

mjoin :: (Monoid m) => m -> [m] -> m
mjoin sep = mconcat . (intersperse sep)

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

-- Blocks/linebreaks can be used in single-line contexts when wrapped in a
-- div with an extra newline. The TiddlyWiki docs mention this when discussing
-- list items, but it also works for definition lists.
-- https://tiddlywiki.com/#Lists%20in%20WikiText
wrapLinebreaks :: Text -> Text
wrapLinebreaks = surroundBlock2 "<div>\n" "</div>"

isBlockQuote :: Block -> Bool
isBlockQuote (BlockQuote _) = True
isBlockQuote _ = False

isBreak :: Inline -> Bool
isBreak SoftBreak = True
isBreak LineBreak = True
isBreak _ = False

isLineBreak :: Inline -> Bool
isLineBreak LineBreak = True
isLineBreak _ = False

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

data ListStyle = NoStyle | Bullet | Numbered | Nested ListStyle ListStyle

instance Show ListStyle where
    show NoStyle = ""
    show Bullet = "*"
    show Numbered = "#"
    show (Nested first rest) = show rest <> show first

instance Semigroup ListStyle where
    -- The outermost nesting is always the first elem, so flip the argument.
    (<>) = flip Nested

instance Monoid ListStyle where
    mempty = NoStyle

data TiddlyList = TiddlyList ListStyle [[Block]]

writeList :: PandocMonad m => TiddlyList -> TiddlyWiki m Text
writeList = writeNestedList mempty

writeNestedList :: PandocMonad m => ListStyle -> TiddlyList -> TiddlyWiki m Text
writeNestedList styles (TiddlyList style bss) =
    mconcat . map (<> "\n") <$> mapM writeItem bss
    where writeItem = writeListItem (styles <> style)

writeListItem :: PandocMonad m => ListStyle -> [Block] -> TiddlyWiki m Text
writeListItem _ [] = return mempty
-- Single lines can be formatted using the simple syntax.
writeListItem styles [b] | isSingleLine b =
    prependStyle <$> writeBlock b
    where prependStyle onto = T.pack (show styles) <> " " <> onto
writeListItem styles [(BulletList bss)] =
    writeNestedList styles $ TiddlyList Bullet bss
writeListItem styles [(OrderedList _ bss)] =
    writeNestedList styles $ TiddlyList Numbered bss
writeListItem styles bs =
    wrap <$> writeBlocks bs
    where wrap x = T.pack (show styles) <> " " <> wrapLinebreaks x

data BlockStyle = NoBlockStyle
                | OnlyClasses [Text]
                | OnlyCSSStyle Text
                | RawStyle Attr

toBlockStyle :: Attr -> BlockStyle
toBlockStyle s | s == nullAttr = NoBlockStyle
toBlockStyle (iden, cls, []) | T.null iden = OnlyClasses cls
toBlockStyle (iden, [], [("style", s)]) | T.null iden = OnlyCSSStyle s
toBlockStyle attr = RawStyle attr

writeDiv :: BlockStyle -> Text -> Text
writeDiv NoBlockStyle = id
writeDiv (OnlyClasses cs) =
    surroundBlock2 ("@@" <> (classString cs)) "@@"
    where classString = (mjoin mempty) . map ("." <>)
writeDiv (OnlyCSSStyle style) =
    surroundBlock2 ("@@" <> (condense style)) "@@"
    where condense = T.pack . filter isSpace . T.unpack
writeDiv (RawStyle attr) =
    L.toStrict . renderHtml . wrap
    where wrap = (H.div ! (toAttribute attr)) . H.preEscapedToHtml

writePara :: (PandocMonad m) => [Inline] -> TiddlyWiki m Text
writePara is
    | any isLineBreak is = do
        body <- local enterLineBreakBlock (writeInlines is)
        return $ surroundBlock "\"\"\"" body
    | otherwise = writeInlines is

writeBlocks :: PandocMonad m => [Block] -> TiddlyWiki m Text
writeBlocks =
    (fmap joinBlocks) . mapM writeBlock
    where joinBlocks = mjoin "\n\n" . filter (not . T.null)

writeBlock :: PandocMonad m => Block -> TiddlyWiki m Text

writeBlock (Plain inlines) = writePara inlines
writeBlock (Para inlines) = writePara inlines

-- https://tiddlywiki.com/#Hard%20Linebreaks%20in%20WikiText
writeBlock (LineBlock iss) =
    fmap (surroundBlock "\"\"\"") . body $ iss
    where body = fmap (mjoin "\n") . mapM writeInlines

writeBlock (CodeBlock attr text) =
    return . writeDiv (toBlockStyle attr) . surroundBlock "```" $ text

-- https://tiddlywiki.com/#HTML%20in%20WikiText
writeBlock (RawBlock f text)
    | f == Format "html" = return text
    | f == Format "tiddlywiki" = return text
writeBlock b@(RawBlock _ _) = T.empty <$ report (BlockNotRendered b)

writeBlock (BlockQuote bs) =
    do
        inQuote <- asks inBlockQuote
        if inQuote then indent "> " <$> writeBlocks bs
                   else surroundBlock "<<<" <$> local enterBlockQuote (writeBlocks bs)

-- TODO(jkz): Figure out if TiddlyWiki can support anything besides
-- numbered lists.
writeBlock (OrderedList _ bss) = writeList $ TiddlyList Numbered bss

writeBlock (BulletList bss) = writeList $ TiddlyList Bullet bss

-- Contrary to Pandoc terminology, TiddlyWiki does not consider definitions
-- to really be lists at all, so they use a totally different syntax.
writeBlock (DefinitionList terms) =
    mconcat . (intersperse "\n") <$> mapM writeItem terms
    where writeItem (is, bss) =
            do term <- writeTerm is
               definitions <- writeDefinitions bss
               return $ "; " <> term <> "\n" <> definitions
          writeTerm is
            | not $ any isBreak is = writeInlines is
            | otherwise            = wrapLinebreaks <$> writeInlines is
          writeDefinition [b] | isSingleLine b = writeBlock b
          writeDefinition bs = wrapLinebreaks <$> writeBlocks bs
          writeDefinitions = fmap (mjoin "\n" . map (": " <>)) . mapM writeDefinition

-- https://tiddlywiki.com/#Headings%20in%20WikiText
writeBlock (Header level attr inlines) =
    header (toBlockStyle attr) <$> writeInlines inlines
    -- XXX(jkz): Should we support #ids? There's not a clean way to translate
    -- ids, and many formats *always* provide ids. Maybe we can match the ID
    -- against the generated header?
    where header (OnlyClasses cs) =
              prefix (mjoin mempty . map ("." <>) $ cs)
          header _ = prefix mempty
          prefix c v = mrepeated level "!" <> c <> " " <> v

-- https://tiddlywiki.com/#Horizontal%20Rules%20in%20WikiText
writeBlock HorizontalRule = return "---"

-- TODO(jkz): Implement tables.
writeBlock b@(Table{}) = T.empty <$ report (BlockNotRendered b)

writeBlock (Div a bs) = writeDiv (toBlockStyle a) <$> writeBlocks bs

writeBlock Null = return T.empty

isNormalText :: Inline -> Bool
isNormalText (Str _) = True
isNormalText Space = True
isNormalText _ = False

-- | Convert a Pandoc Attr into a Blaze Attribute that can be added to an
-- | Html element.
toAttribute :: Attr -> H.Attribute
toAttribute (identifier, classes, kvs) =
    mconcat . (++ extraAttributes) . catMaybes $ [ idAttribute
                                                 , classAttribute ]
    where idAttribute =
            if (not . T.null) identifier
                then Just . A.id . H.textValue $ identifier
                else Nothing
          classAttribute =
              case classes of
                  [] -> Nothing
                  cs -> Just . A.class_ . H.textValue . (mjoin "\n") $ cs
          extraAttribute (k, v) =
              H.customAttribute (tag k) (H.textValue v)
              where tag = fromString . T.unpack
          extraAttributes = map extraAttribute kvs

writeInlines :: PandocMonad m => [Inline] -> TiddlyWiki m Text
writeInlines inlines =
    (fmap mconcat) . mapM writeInline $ inlines

writeInline :: PandocMonad m => Inline -> TiddlyWiki m Text

writeInline (Str t) = return t

writeInline (Emph is) = surround "//" <$> writeInlines is
writeInline (Underline is) = surround "__" <$> writeInlines is
writeInline (Strong is) = surround "''" <$> writeInlines is
writeInline (Strikeout is) = surround "~~" <$> writeInlines is
writeInline (Superscript is) = surround "^^" <$> writeInlines is
writeInline (Subscript is) = surround ",," <$> writeInlines is

writeInline (SmallCaps is) =
    -- TiddlyWiki markup doesn't support small caps, so we wrap it in a
    -- small-caps style.
    surroundBlock2 "@@font-variant-caps: small-caps;" "@@" <$> writeInlines is

-- TODO(jkz): Support Quoted.
writeInline i@Quoted{} = T.empty <$ report (InlineNotRendered i)

-- TODO(jkz): Support Cite.
writeInline i@Cite{} = T.empty <$ report (InlineNotRendered i)

writeInline (Code _ code)
    -- Use double-quotes if the code contains a backtick.
    | '`' `elem` (T.unpack code) = return $ surround "``" code
    | otherwise                  = return $ surround "`" code

writeInline Space = return " "
writeInline SoftBreak = do
    supportsLinebreak <- asks inLineBreakBlock
    -- In line break blocks, translate soft break as space, to avoid introducing
    -- additional hard line breaks.
    return $ if supportsLinebreak then " " else "\n"

writeInline i@LineBreak = do
    supportsLinebreak <- asks inLineBreakBlock
    if supportsLinebreak
        then return "\n"
        else T.empty <$ report (InlineNotRendered i)

-- TODO(jkz): Support Math.
writeInline i@Math{} = T.empty <$ report (InlineNotRendered i)

-- HTML is rendered direclty inline https://tiddlywiki.com/#HTML%20in%20WikiText
writeInline (RawInline f t)
    | f == Format "html" = return t
    | f == Format "tiddlywiki" = return t
writeInline i@(RawInline _ _) = T.empty <$ report (InlineNotRendered i)

writeInline (Link attr is (url, _)) =
    -- Use <a> if there is non-trivial formatting in the body text, or we
    -- want to add link attributes.
    if all isNormalText is && nullAttr == attr
        -- TODO(jkz): For wiki links, remove the | part if body == url
        then wiki <$> inlineText
        else wrapHtml <$> inlineText
    where inlineText = writeInlines is
          wrapA = (H.a ! A.href (H.textValue url) ! toAttribute attr)
                  . H.preEscapedToHtml
          wrapHtml = L.toStrict . renderHtml . wrapA
          wiki body = "[[" <> body <> "|" <> url <> "]]"

-- TODO(jkz): Support attrs for image including height/width.
writeInline i@(Image _ is (url, _))
    | all isNormalText is =
        img <$> writeInlines is
    | otherwise           = T.empty <$ report (InlineNotRendered i)
    where img title = "[img[" <> title <> "|" <> url <> "]]"

-- TODO(jkz): Support Note.
writeInline i@Note{} = T.empty <$ report (InlineNotRendered i)

writeInline (Span attr is) =
    L.toStrict
    . renderHtml
    . (H.span ! (toAttribute attr))
    . H.preEscapedText
    <$> writeInlines is