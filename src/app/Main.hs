module Main where

import qualified Markdown as Markdown

main :: IO ()
main = do
  raw <- readFile "test.md"
  let 
    document :: Markdown.Document
    document = Markdown.parse raw

    -- parsed :: Markdown.Document
    -- parsed = [Markdown.OrderedList[
    --   Markdown.Paragraph[Markdown.Plain "Item 1"],
    --   Markdown.Paragraph[Markdown.Plain "Item 2"]]]

  -- putStrLn raw
  putStrLn $ Markdown.documentToHtml document
