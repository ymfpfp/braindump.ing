module Extensions where

import Data.Char (isAlphaNum, toLower, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map as Map
import qualified Markdown as Md

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseFrontmatter :: Md.Extension
parseFrontmatter doc = case doc of
  Md.HorizontalBreak : rest
    | (fields, Md.HorizontalBreak : body) <- break isBreak rest ->
      (body, Map.fromList (concatMap toEntry fields))
  _ -> (doc, Map.empty)

  where 
    isBreak :: Md.Block -> Bool
    isBreak Md.HorizontalBreak = True
    isBreak _ = False

    toEntry :: Md.Block -> [(String, Md.Document)]
    toEntry (Md.Paragraph inline) = 
      case break (== ':') (concatMap Md.inlineToText inline) of
        (key, ':' : value) -> [(trim key, [Md.Fragment [Md.Plain $ trim value]])]
        _ -> []
    toEntry _ = []

parseTOC :: Md.Extension
parseTOC doc = (map anchor doc, Map.singleton "toc" toc)
  where
    -- Give each heading an id matching the TOC link's target.
    anchor (Md.Heading (depth, inline, props)) =
      Md.Heading (depth, inline, props ++ [("id", slug (concatMap Md.inlineToText inline))])
    anchor block = block

    toc :: Md.Document
    toc = map
        (\(Md.Heading (depth, inline, props)) -> 
          Md.Heading (
            depth,
            [Md.Link ('#' : slug (concatMap Md.inlineToText inline), inline)],
            props
          )
        )
        (filter isHeading doc)

    isHeading :: Md.Block -> Bool
    isHeading (Md.Heading (_, _, _)) = True
    isHeading _ = False

    slug :: String -> String
    slug =
      map (\c -> if c == ' ' then '-' else c)
        . filter (\c -> isAlphaNum c || c == ' ')
        . map toLower
        . trim

-- TODO
-- Footnotes come in two flavours:
--
--   * A *reference* sits inline somewhere in the prose, e.g. `...the server.[^1]`.
--   * A *definition* is a top-level paragraph that opens with `[^1]` (the colon is
--     optional), e.g. `[^1]: I have a spare laptop at home.`
--
-- Rather than collecting footnotes into a separate section, we attach each one
-- directly to the block that references it: the `[^id]` marker becomes a `<sup>`,
-- and the definition is dropped in at the front of the block as an `aside` span.
-- So `hi[^1]` followed by `[^1]: this is a test` renders as
-- `<p><span class="aside">(1) this is a test</span>hi<sup>^1</sup></p>`.
--
-- Definitions are matched to references *positionally* — a definition belongs to
-- the block it immediately follows — because the same label (`[^1]`) gets reused
-- for different notes throughout a post.
--
-- The inline parser shreds `[^1]` into two adjacent `Plain` fragments: a `"[^1"`
-- (since `[` always begins a fresh `Plain`) followed by a `"]..."`. So rather than
-- stitching fragments back together, we just match that pair directly.
parseFootnotes :: Md.Extension
parseFootnotes doc = (attach doc, Map.empty)
  where
    -- Walk blocks left to right, attaching each block's footnotes (the run of
    -- definition paragraphs that immediately follow it) and dropping the now
    -- consumed definitions.
    attach :: Md.Document -> Md.Document
    attach [] = []
    attach (block : rest)
      | Just _ <- asDefinition block = attach rest -- orphan definition, drop it
      | otherwise =
        let (defs, rest') = span isDefinition rest
        in withFootnotes (map definition defs) block : attach rest'

    isDefinition :: Md.Block -> Bool
    isDefinition block = case asDefinition block of
      Just _ -> True
      Nothing -> False

    definition :: Md.Block -> (String, [Md.Inline])
    definition block = case asDefinition block of
      Just def -> def
      -- Unreachable: `attach` only feeds `definition` blocks it already matched.
      Nothing -> ("", [])

    -- A definition is a paragraph opening with the `[^id]` marker — a `"[^id"`
    -- fragment, then a `"]..."` one — yielding the label and the note's content.
    asDefinition :: Md.Block -> Maybe (String, [Md.Inline])
    asDefinition (Md.Paragraph (Md.Plain ('[' : '^' : label) : Md.Plain (']' : after) : rest))
      | not (null label), all isAlphaNum label =
        let content = dropWhile isSpace (dropWhile (== ':') after)
        in Just (label, [Md.Plain content | not (null content)] ++ rest)
    asDefinition _ = Nothing

    -- Turn `[^id]` markers into superscripts, then prepend each note as an aside.
    withFootnotes :: [(String, [Md.Inline])] -> Md.Block -> Md.Block
    withFootnotes notes = prepend (map aside notes) . mapInlines convertRefs

    aside :: (String, [Md.Inline]) -> Md.Inline
    aside (label, content) =
      Md.Span (Md.Plain ("(" ++ label ++ ") ") : content, [("class", "aside")])

-- Prepend inlines to a block's own content (paragraphs and headings carry refs).
prepend :: [Md.Inline] -> Md.Block -> Md.Block
prepend [] block = block
prepend asides block = case block of
  Md.Paragraph inlines -> Md.Paragraph (asides ++ inlines)
  Md.Heading (depth, inlines, props) -> Md.Heading (depth, asides ++ inlines, props)
  other -> other

-- Replace each `[^id]` marker — a `"[^id"` fragment followed by a `"]..."` one —
-- with a `<sup>id</sup>` superscript, recursing through nested inlines.
convertRefs :: [Md.Inline] -> [Md.Inline]
convertRefs inlines = case inlines of
  (Md.Plain ('[' : '^' : label) : Md.Plain (']' : after) : rest)
    | not (null label), all isAlphaNum label ->
      Md.Plain ("<sup>" ++ label ++ "</sup>") : convertRefs (Md.Plain after : rest)
  (x : rest) -> recurse x : convertRefs rest
  [] -> []
  where
    recurse (Md.Italic nested) = Md.Italic (convertRefs nested)
    recurse (Md.Bold nested) = Md.Bold (convertRefs nested)
    recurse (Md.Link (href, nested)) = Md.Link (href, convertRefs nested)
    recurse (Md.Span (nested, props)) = Md.Span (convertRefs nested, props)
    recurse other = other

-- Apply an inline transformation everywhere inlines appear within a block.
mapInlines :: ([Md.Inline] -> [Md.Inline]) -> Md.Block -> Md.Block
mapInlines f block = case block of
  Md.Paragraph inlines -> Md.Paragraph (f inlines)
  Md.Heading (depth, inlines, props) -> Md.Heading (depth, f inlines, props)
  Md.Fragment inlines -> Md.Fragment (f inlines)
  Md.Blockquote blocks -> Md.Blockquote (map (mapInlines f) blocks)
  Md.OrderedList items -> Md.OrderedList (map (map (mapInlines f)) items)
  Md.UnorderedList items -> Md.UnorderedList (map (map (mapInlines f)) items)
  other -> other
