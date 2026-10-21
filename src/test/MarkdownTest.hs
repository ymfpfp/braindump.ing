module Main where

import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import qualified Markdown as Md
import System.Process (readProcess)

render :: String -> String
render = Md.documentToHtml . Md.parse

assertEqual :: String -> String -> String -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    fail (label ++ "\nExpected: " ++ expected ++ "\nActual: " ++ actual)

main :: IO ()
main = do
  let cases =
        [ ("identifier", "Efabless_sky130", "Efabless_sky130")
        , ("multiple underscores", "sky130_fd_sc_hd__and2_2", "sky130_fd_sc_hd__and2_2")
        , ("Unicode identifier", "caf\233_name", "caf\233_name")
        , ("underscore italics", "_italic_", "<i>italic</i>")
        , ("asterisk italics", "*italic*", "<i>italic</i>")
        , ("italic identifier", "_foo_bar_", "<i>foo_bar</i>")
        , ("bold", "**bold**", "<b>bold</b>")
        , ("escaped underscore", "Efabless\\_sky130", "Efabless_sky130")
        , ("code link", "[`Efabless_sky130`](https://example.com/)",
           "<a href=\"https://example.com/\"><code>Efabless_sky130</code></a>")
        , ("italic link", "[_Markdown.hs_](https://example.com/)",
           "<a href=\"https://example.com/\"><i>Markdown.hs</i></a>")
        ]
  forM_ cases $ \(label, source, expected) ->
    assertEqual label ("<p>" ++ expected ++ "</p>") (render (source ++ "\n"))

  let packageLink = "[\"Efabless_sky130 1.0.7\"](https://github.com/efabless/sky130_klayout_pdk/)"
      paragraph = "Searching " ++ packageLink ++ " and installing it.\n"
      following = "\nThe rest of the article has _normal italics_.\n"
      expected = "<p>Searching <a href=\"https://github.com/efabless/sky130_klayout_pdk/\">\"Efabless_sky130 1.0.7\"</a> and installing it.</p>"
              ++ "<p>The rest of the article has <i>normal italics</i>.</p>"
  assertEqual "package link keeps following paragraphs intact" expected (render (paragraph ++ following))
  formatted <- prettier (paragraph ++ following)
  assertEqual "package link after Prettier" expected (render formatted)

  -- Exercise the complete article with its ordinary package-name link label.
  article <- readFile "../routes/writing/2026-09-04-asic-puzzle.md"
  let html = render article
  unless ("<a href=\"https://github.com/efabless/sky130_klayout_pdk/\">\"Efabless_sky130 1.0.7\"</a> and installing it.</p>" `isInfixOf` html) $
    fail "The article's package link is broken"
  unless ("<p>We can pull that script" `isInfixOf` html) $
    fail "The paragraph after the package link was swallowed"
  formattedArticle <- prettier article
  assertEqual "complete article after Prettier" html (render formattedArticle)
  putStrLn "Markdown regression checks passed, including the article after Prettier."

prettier :: String -> IO String
prettier = readProcess "../node_modules/.bin/prettier" ["--parser", "markdown"]
