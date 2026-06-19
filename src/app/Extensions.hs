module Extensions where

import Data.Char (isAlphaNum, toLower)
import qualified Data.Map as Map
import qualified Markdown as Md

-- parseFrontmatter :: Md.Extension

-- Collect every top-level `Heading` and emit a table of contents as an extra
-- under the "toc" key. Each heading is also given a matching anchor id (computed
-- here with `slug`, so the slug logic lives in one place). 
parseTOC :: Md.Extension
parseTOC doc = (map anchor doc, Map.singleton "toc" [toc])
  where
    -- Give each heading an id matching the TOC link's target.
    anchor (Md.Heading (depth, inline, props)) =
      Md.Heading (depth, inline, props ++ [("id", slug (concatMap Md.inlineToText inline))])
    anchor block = block

    toc = Md.UnorderedList $ 
      map
        (\b -> [Md.Paragraph [Md.Link ("#" ++ slug (Md.blockToText b), [Md.Plain (Md.blockToText b)])]])
        (filter isHeading doc)

    isHeading :: Md.Block -> Bool
    isHeading (Md.Heading (_, _, _)) = True
    isHeading _ = False

    slug :: String -> String
    slug =
      map (\c -> if c == ' ' then '-' else c)
        . filter (\c -> isAlphaNum c || c == ' ')
        . map toLower

-- Search the `Document` for [^x] at the beginning of any text
-- parseFootnotes :: Md.Extension
