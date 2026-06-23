module Extensions where

import Data.Char (isAlphaNum, toLower, isSpace)
import Data.List (dropWhileEnd, intercalate, partition)
import qualified Data.Map as Map
import qualified Markdown as Md

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

desc :: Md.Extension
desc doc = (doc, Map.singleton "description" [Md.Fragment [Md.Plain $ summary ++ "..."]])
  where 
    summary = trim $ intercalate " " (take 50 $ words $ Md.documentToText doc)

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
-- right where it's referenced: the `[^id]` marker becomes a `<sup>`, and the
-- definition is dropped in immediately after it as an `aside` span. So `hi[^1]`
-- with a `[^1]: this is a test` definition renders as
-- `<p>hi<sup>1</sup><span class="aside">(1) this is a test</span></p>`.
--
-- References are matched to definitions *by label*: we lift every definition out
-- of the document into a label->aside map, then rewrite references by lookup. A
-- definition can therefore sit anywhere — often at the end of the post, far from
-- the reference it belongs to.
--
-- The inline parser shreds `[^1]` into two adjacent `Plain` fragments: a `"[^1"`
-- (since `[` always begins a fresh `Plain`) followed by a `"]..."`. So rather than
-- stitching fragments back together, we just match that pair directly.
parseFootnotes :: Md.Extension
parseFootnotes doc = (map (mapInlines (convertRefs notes)) body, Map.empty)
  where
    -- Lift definitions (top-level paragraphs opening with `[^id]`) out of the
    -- document, leaving the rest as the body and keying their asides by label.
    (defs, body) = partition isDefinition doc

    notes :: Map.Map String Md.Inline
    notes = Map.fromList
      [ (label, aside label content)
      | def <- defs
      , Just (label, content) <- [asDefinition def]
      ]

    isDefinition :: Md.Block -> Bool
    isDefinition block = case asDefinition block of
      Just _ -> True
      Nothing -> False

    -- A definition is a paragraph opening with the `[^id]` marker — a `"[^id"`
    -- fragment, then a `"]..."` one — yielding the label and the note's content.
    asDefinition :: Md.Block -> Maybe (String, [Md.Inline])
    asDefinition (Md.Paragraph (Md.Plain ('[' : '^' : label) : Md.Plain (']' : after) : rest))
      | not (null label), all isAlphaNum label =
        let content = dropWhile isSpace (dropWhile (== ':') after)
        in Just (label, [Md.Plain content | not (null content)] ++ rest)
    asDefinition _ = Nothing

    aside :: String -> [Md.Inline] -> Md.Inline
    aside label content =
      Md.Span (Md.Plain ("(" ++ label ++ ") ") : content, [("class", "aside")])

-- Replace each `[^id]` reference — a `"[^id"` fragment followed by a `"]..."` one —
-- with a `<sup>id</sup>`, dropping the matching note's aside in right after it (if
-- the label has a definition). Recurses through nested inlines.
convertRefs :: Map.Map String Md.Inline -> [Md.Inline] -> [Md.Inline]
convertRefs notes inlines = case inlines of
  (Md.Plain ('[' : '^' : label) : Md.Plain (']' : after) : rest)
    | not (null label), all isAlphaNum label ->
      let sup = Md.Plain ("<sup>" ++ label ++ "</sup>")
          here = maybe [] (: []) (Map.lookup label notes)
      in sup : here ++ convertRefs notes (Md.Plain after : rest)
  (x : rest) -> recurse x : convertRefs notes rest
  [] -> []
  where
    recurse (Md.Italic nested) = Md.Italic (convertRefs notes nested)
    recurse (Md.Bold nested) = Md.Bold (convertRefs notes nested)
    recurse (Md.Link (href, nested)) = Md.Link (href, convertRefs notes nested)
    recurse (Md.Span (nested, props)) = Md.Span (convertRefs notes nested, props)
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
