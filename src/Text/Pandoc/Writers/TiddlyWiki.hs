{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.TiddlyWiki ( writeTiddlyWiki ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Options (WriterOptions (writerHTMLMathMethod, writerPreferAscii), HTMLMathMethod(..))
import Text.Pandoc.Definition
import Text.Pandoc.Logging (LogMessage(..))
import Data.List (intersperse)
import Control.Monad.Reader (ReaderT(runReaderT), local, asks)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Maybe (catMaybes)
import Data.String (IsString(..))
import Data.Char (isSpace, ord)
import qualified Text.TeXMath as TeX
import Text.Pandoc.Writers.Math (convertMath)
import Text.XML.Light.Output (ppElement, showElement)

data Environment =
    Environment { inBlockQuote :: Bool
                , inLineBreakBlock :: Bool
                , inHtml :: Bool
                , skipSoftBreak :: Bool
                , writerOptions :: WriterOptions
                }

enterBlockQuote :: (PandocMonad m) => TiddlyWiki m a -> TiddlyWiki m a
enterBlockQuote = local (\e -> e { inBlockQuote = True })

enterLineBreakBlock :: (PandocMonad m) => TiddlyWiki m a -> TiddlyWiki m a
enterLineBreakBlock = local (\e -> e { inLineBreakBlock = True })

enterHtml :: (PandocMonad m) => TiddlyWiki m a -> TiddlyWiki m a
enterHtml = local (\e -> e { inHtml = True })

enterListEntry :: (PandocMonad m) => TiddlyWiki m a -> TiddlyWiki m a
enterListEntry = local (\e -> e { skipSoftBreak = True })

type TiddlyWiki m = ReaderT Environment m

runTiddlyWiki :: (PandocMonad m) => WriterOptions -> TiddlyWiki m a -> m a
runTiddlyWiki o r =
    runReaderT r initial
    where initial = Environment { inBlockQuote = False
                                , inLineBreakBlock = False
                                , inHtml = False
                                , skipSoftBreak = False
                                , writerOptions = o
                                }

asksOption :: (Monad m) => (WriterOptions -> a) -> TiddlyWiki m a
asksOption f = asks $ f . writerOptions

-- | Convert Pandoc to TiddlyWiki.
writeTiddlyWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
-- TODO(jkz): Support meta.
writeTiddlyWiki opts (Pandoc _ blocks) =
    runTiddlyWiki opts . writeBlocks $ blocks

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

-- | Escape common WikiText formatting in `Str` elements.
escape :: Text -> Text
escape = escape' "`"
       . escape' "//"
       . escape' "^^"
       . escape' "''"
       . escape' ",,"
       . escape' "~~"
    where escape' t =
            T.replace t ((T.pack . mconcat . map charRef . T.unpack) t)
          charRef x = "&#" <> (show . ord $ x) <> ";"

-- Blocks/linebreaks can be used in single-line contexts when wrapped in a
-- div with an extra newline. The TiddlyWiki docs mention this when discussing
-- list items, but it also works for definition lists.
-- https://tiddlywiki.com/#Lists%20in%20WikiText
wrapLinebreaks :: Text -> Text
wrapLinebreaks = surroundBlock2 "<div>\n" "</div>"

isHardBreak :: Inline -> Bool
isHardBreak LineBreak = True
isHardBreak (Math DisplayMath _) = True
isHardBreak _ = False

isLineBreak :: Inline -> Bool
isLineBreak LineBreak = True
isLineBreak _ = False

-- | Evaluates if the given block can be written as a single line. Only
-- single-line blocks, or blocks with less than 3 SoftBreaks are legal list
-- elements.
canBeSingleLine :: Block -> Bool
canBeSingleLine =
    maybe False (not . needsBreak) . inlines
    where inlines (Plain is) = Just is
          inlines (Para is) = Just is
          inlines _ = Nothing
          needsBreak is = any isHardBreak is || tooManySoftBreaks is
          -- True if there are more than 3 soft breaks.
          tooManySoftBreaks = (>= 3) . sum . map (fromEnum . isSoftBreak)
          isSoftBreak SoftBreak = True
          isSoftBreak _ = False

-- | Evaluates if this block is a list.
isList :: Block -> Bool
isList BulletList{} = True
isList OrderedList{} = True
isList _ = False

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
    -- Appending NoStyle is meaningless, so just ignore it.
    (<>) l NoStyle = l
    -- The outermost nesting is always the first elem, so flip the argument.
    (<>) l r = Nested r l

instance Monoid ListStyle where
    mempty = NoStyle

data TiddlyList = TiddlyList ListStyle [[Block]]

-- | TiddlyWiki's list syntax only supports blocks without line breaks, and
-- other nested lists. Some readers translate nested lists as
--  <ListType> .. [[(Para ...), (<ListType> ...)]]
-- That is to say, a single list item with multiple blocks, a Plain/Para and
-- a nested list. This function detects if such lists are convertable to native
-- TiddlyWiki Syntax, or if they need to use the less user-friendly extended
-- syntax.
isListTiddlyCompatible :: [[Block]] -> Bool
isListTiddlyCompatible [] = True
isListTiddlyCompatible ([] : bss) = isListTiddlyCompatible bss
isListTiddlyCompatible ([b] : bss)
    | canBeSingleLine b = isListTiddlyCompatible bss
    | isList b = isListTiddlyCompatible bss
-- Paragraph followed by a nested list.
isListTiddlyCompatible ([b1, b2] : bss)
    | canBeSingleLine b1 && isList b2 = isListTiddlyCompatible bss
isListTiddlyCompatible _ = False

writeList :: PandocMonad m => TiddlyList -> TiddlyWiki m Text
writeList = writeNestedList mempty

-- | Write the given TiddlyList with no special case handling. This avoids the
-- infinite recursion caused by isListTiddlyCompatible (since the
-- isListTiddlyWikiCompatible) case recurses on a tiddly-compatible list.
writeNestedListNoSpecial :: PandocMonad m => ListStyle -> TiddlyList -> TiddlyWiki m Text
writeNestedListNoSpecial styles (TiddlyList style bss) =
    mjoin "\n" <$> mapM writeItem bss
    where writeItem = writeListItem (styles <> style)

writeNestedList :: PandocMonad m => ListStyle -> TiddlyList -> TiddlyWiki m Text
-- Special Case: Some readers translate nested lists as a single item with
-- several blocks. Try to detected nested cases that could be translated to
-- TiddlyWiki's nested list syntax, and treat them like lists of serveral items.
writeNestedList styles l@(TiddlyList style bss)
    | isListTiddlyCompatible bss =
        -- Tiddly-compatible lists are equivalent to a single list, with each
        -- item as a single block from the original list.
        writeNestedListNoSpecial styles . TiddlyList style . map pure . concat $ bss
    | otherwise                  = writeNestedListNoSpecial styles l

writeListItem :: PandocMonad m => ListStyle -> [Block] -> TiddlyWiki m Text
writeListItem _ [] = return mempty
-- Single lines can be formatted using the simple syntax.
writeListItem styles [b] | canBeSingleLine b =
    prependStyle <$> enterListEntry (writeBlock b)
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
        body <- enterLineBreakBlock . writeInlines $ is
        return $ surroundBlock "\"\"\"" body
    | otherwise = writeInlines is

wrapQuote :: PandocMonad m => QuoteType -> Text -> TiddlyWiki m Text
wrapQuote SingleQuote v = do
    preferAscii <- asksOption writerPreferAscii
    return $ if preferAscii then "'" <> v <> "'"
                            else "\8216" <> v <> "\8217"
wrapQuote DoubleQuote v = do
    preferAscii <- asksOption writerPreferAscii
    return $ if preferAscii then "\"" <> v <> "\""
                            else "\8220" <> v <> "\8221"

writeMathML :: PandocMonad m => MathType -> Text -> TiddlyWiki m Text
writeMathML type_ math = do
    converted <- convertMath TeX.writeMathML type_ math
    case converted of
        Right e -> return . T.pack $ case type_ of
            InlineMath -> showElement e
            DisplayMath -> ppElement e
        Left i -> writeInline i

writeMath :: PandocMonad m => MathType -> Text -> TiddlyWiki m Text
writeMath type_ m = do
    mode <- asksOption writerHTMLMathMethod
    case mode of
        MathML -> writeMathML type_ m
        -- Based on the Syntax of https://tiddlywiki.com/#KaTeX%20Plugin
        KaTeX _ -> return $ case type_ of
            InlineMath -> surround "$$" m
            DisplayMath -> surroundBlock "$$" m
        _ -> return . surround "`" $ m

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
                   else surroundBlock "<<<" <$> enterBlockQuote (writeBlocks bs)

-- TiddlyWiki only supports numeric lists that start from 1. So ignore
-- the ListAttributes.
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
            | not $ any isHardBreak is = enterListEntry . writeInlines $ is
            | otherwise            = wrapLinebreaks <$> writeInlines is
          writeDefinition [b] | canBeSingleLine b = enterListEntry . writeBlock $ b
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

writeInline (Str t) = do
    withinHtml <- asks inHtml
    -- Skip escaping if we're in an HTML context.
    return $ if withinHtml then t else escape t

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

writeInline (Quoted style is) = do
    inner <- writeInlines is
    wrapQuote style inner

-- TiddlyWiki doesn't natively support citations, and I'm not aware of any major
-- footnote/ref plugins, so skip cites.
writeInline i@Cite{} = T.empty <$ report (InlineNotRendered i)

writeInline (Code _ code)
    -- Use double-quotes if the code contains a backtick.
    | '`' `elem` (T.unpack code) = return $ surround "``" code
    | otherwise                  = return $ surround "`" code

writeInline Space = return " "
writeInline SoftBreak = do
    skip <- asks skipSoftBreak
    supportsLinebreak <- asks inLineBreakBlock
    -- In line break blocks, translate soft break as space, to avoid introducing
    -- additional hard line breaks.
    return $ if skip || supportsLinebreak then " " else "\n"

writeInline i@LineBreak = do
    supportsLinebreak <- asks inLineBreakBlock
    if supportsLinebreak
        then return "\n"
        else T.empty <$ report (InlineNotRendered i)

writeInline (Math type_ m) = writeMath type_ m

-- HTML is rendered direclty inline https://tiddlywiki.com/#HTML%20in%20WikiText
writeInline (RawInline f t)
    | f == Format "html" = return t
    | f == Format "tiddlywiki" = return t
writeInline i@(RawInline _ _) = T.empty <$ report (InlineNotRendered i)

writeInline (Link attr is (url, _)) =
    -- Use <a> if there is non-trivial formatting in the body text, or we
    -- want to add link attributes.
    if all isNormalText is && nullAttr == attr
        then wiki <$> inlineText
        else wrapHtml <$> enterHtml inlineText
    where inlineText = writeInlines is
          wrapA = (H.a ! A.href (H.textValue url) ! toAttribute attr)
                  . H.preEscapedToHtml
          wrapHtml = L.toStrict . renderHtml . wrapA
          wiki body
            | body == url = "[[" <> url <> "]]"
            | otherwise   = "[[" <> body <> "|" <> url <> "]]"

-- TODO(jkz): Support attrs for image including height/width.
writeInline i@(Image _ is (url, _))
    | all isNormalText is =
        img <$> writeInlines is
    | otherwise           = T.empty <$ report (InlineNotRendered i)
    where img title = "[img[" <> title <> "|" <> url <> "]]"

-- TiddlyWiki doesn't natively support notes, and I'm not aware of any major
-- footnote/ref plugins, so skip notes.
writeInline i@Note{} = T.empty <$ report (InlineNotRendered i)

writeInline (Span attr is) = L.toStrict
                           . renderHtml
                           . (H.span ! (toAttribute attr))
                           . H.preEscapedText
                           <$> enterHtml (writeInlines is)