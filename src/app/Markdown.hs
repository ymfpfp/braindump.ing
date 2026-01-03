module Markdown where

import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Parser

type Document = [Block]

data Block
  = Paragraph [Inline]
  | Heading (Int, [Inline])
  | Blockquote [Block]
  | OrderedList [[Block]]
  | UnorderedList [[Block]]
  | Image (String, String)
  | HorizontalBreak 
  | CodeBlock (String, String)
  deriving Show

data Inline
  = Plain String
  | Italic [Inline]
  | Bold [Inline]
  | Code String
  | Link (String, [Inline])
  deriving Show

documentToHtml :: Document -> String
-- `foldr` is basically `reduce`, folding the given list from the right.
documentToHtml = foldr (\block html -> blockToHtml block ++ html) ""

documentToText :: Document -> String
documentToText = foldr (\block text -> blockToText block ++ text) ""

wrap :: String -> String -> String
wrap tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

wrapWithProps :: String -> [(String, String)] -> String -> String
wrapWithProps tag props content = 
  "<" ++ tag ++ concatMap format props ++ ">" ++ content ++ "</" ++ tag ++ ">"
  where 
    format (k, v) = " " ++ k ++ "=\"" ++ v ++ "\""

blockToHtml :: Block -> String
blockToHtml block = case block of
  Paragraph inline -> wrap "p" $ concatMap inlineToHtml inline
  Heading (depth, inline) -> 
    wrap ("h" ++ show depth) $ concatMap inlineToHtml inline
  Blockquote blocks -> wrap "blockquote" $ concatMap blockToHtml blocks
  OrderedList items -> wrap "ol" $ concatMap (wrap "li" . documentToHtml) items
  UnorderedList items -> wrap "ul" $ concatMap (wrap "li" . documentToHtml) items
  Image (alt, src) -> wrapWithProps "img" [("alt", alt), ("src", src)] ""
  HorizontalBreak -> "<hr>"
  CodeBlock (language, code) -> 
    wrap "pre" $ wrapWithProps "code" [("class", "language-" ++ language)] code

blockToText :: Block -> String
blockToText block = case block of
  Paragraph inline -> concatMap inlineToText inline
  Heading (_, inline) -> concatMap inlineToText inline
  Blockquote blocks -> concatMap blockToText blocks
  OrderedList items -> concatMap documentToText items
  UnorderedList items -> concatMap documentToText items
  Image (alt, _) -> alt
  HorizontalBreak -> ""
  CodeBlock (_, code) -> code

inlineToHtml :: Inline -> String
inlineToHtml inline = case inline of
  Plain s -> s
  Italic nested -> wrap "i" $ concatMap inlineToHtml nested
  Bold nested -> wrap "b" $ concatMap inlineToHtml nested
  Code code -> wrap "code" code
  Link (link, nested) -> 
    wrapWithProps "a" [("href", link)] $ concatMap inlineToHtml nested

inlineToText :: Inline -> String
inlineToText inline = case inline of
  Plain s -> s
  Italic nested -> concatMap inlineToText nested
  Bold nested -> concatMap inlineToText nested
  -- Ignore code.
  Code code -> code
  Link (_, nested) -> concatMap inlineToText nested

-- Also support extensions.
type Extension = Document -> (Document, Map String Document)

-- The actual parsers. 
parse :: String -> Document 
-- Either return `[]` or the `[Block]` part of the final `Just (blocks, stream)`.
parse raw = maybe [] fst ((runParser $ many parseBlock) raw)

parseWithExtensions :: String -> [Extension] -> (Document, Map String Document)
parseWithExtensions raw [] = (parse raw, Map.empty)
parseWithExtensions raw extensions =
  foldr (\extension (doc, extras) -> 
    let (out, extra) = extension doc in (out, Map.union extras extra))
    (parse raw, Map.empty) extensions

parseBlock :: Parser Block
parseBlock = do
  -- Parse as many newlines first.
  _ <- many $ satisfy (\c -> c == '\n')
  parseOrderedList <|> parseUnorderedList <|> parseCodeBlock <|> 
   parseHorizontalBreak <|> parseImage <|> parseBlockquote <|> 
    parseHeading <|> parseParagraph

-- TODO: Need to figure on \n\n vs \n.
parseParagraph :: Parser Block
parseParagraph = Paragraph <$> manyUntil (match "\n") parseInline

-- I wrote this parser based on monadic chaining first, so I'll try to explain it.
-- We are still chaining together functions, but monads, along with Haskell's `do`
-- syntactical sugar, let us chain them really cleanly.
--
-- From bottom-up, `return` = `pure`, meaning we return a parser that 
-- returns a `Heading`. So clearly we are chaining functions. This desugars down to:
--
-- ```
-- parseHeading = 
--   length <$> some (match "#") >>= \depth ->
--     match " " >>= \_ ->
--       some parseInline >>= \nested ->
--         return $ Heading (depth, nested)
-- ```
--
-- So each time we have a `Parser`
--
-- Anyways that's all monads are really about. The phrase 
-- "A monad is a monoid in the category of endofunctors" can roughly desugar down to:
--
-- * Endofunctor: We know a functor is a type that wraps around a value to provide
--   a context and can be mapped over while keeping that context. An endofunctor is
--   that, but with the extra definition that it can map to itself, e.g.
--   `Maybe a -> Maybe b`. Anyways all functors in Haskell are endofunctors.
-- * Monoid: A type class (similar to interfaces in other languages) that enforces
--   three properties: must have: 
--   * An identity context
--   * A binary function we can call `join` that takes two instances of the type and 
--   returns one instance (and if the identity context is passed in, output = other)
--   * Associativity. If the binary function is chained, which pairs are evaluated 
--     first should not matter.
--
-- A list is a monoid, for example:
--
-- * Identity context: []
-- * Binary function: ++, [] ++ [..] -> [..]
-- * Associativity of the binary: ([] ++ [..]) ++ [..] = [] ++ ([..] ++ [..])
--
-- So this sentence means that a monad is a type that enforces all the constraints of
-- a monoid that can map over itself. What is the binary function for a monad though?
-- It's `join :: t (t a) -> t a`, which just merges the context. And you see how 
-- that's an endofunctor, since we can map a monad over itself and use the binary
-- function (which probably has an actual name) to get a third value. And that 
-- flattening should be associative. 
--
-- Thus, a monad is a type that enforces all the constraints of the `Monoid` type class
-- and can be mapped over; it's special power is that flatten nested instances
-- associatively, which enabled chaining. Haskell then provides two very useful
-- constructs, `>>=`, and then `do` which is syntactical sugar on top of `>>=`. `do`
-- makes a function seem almost imperative, when really is is chaining the functions
-- together behind the scenes.
--
-- `join` is 
--
-- E.g. `Maybe`:
--
-- * Identity context: `\a -> Just a`
-- * Binary function: `Maybe (Maybe a) -> Maybe a`, which we know is associative
parseHeading :: Parser Block
parseHeading = do
  depth <- length <$> some (match "#")
  _ <- match " " 
  nested <- some parseInline

  return $ Heading (depth, nested)

parseBlockquote :: Parser Block
parseBlockquote = do
  -- We can start by parsing each line starting with "> " from this point onward,
  -- and then joining them together to use that as the nested stream.
  content <- unlines <$> some parseLines
  case (runParser $ many parseBlock) content of
    Nothing -> empty
    Just (blocks, _) -> return $ Blockquote blocks

  where 
  parseLines :: Parser String
  parseLines = do
    _ <- many $ match " "
    _ <- match ">"
    _ <- Parser.optional $ match " "
    content <- manyUntil (match "\n") (satisfy $ \c -> c /= '\n')
    _ <- Parser.optional $ match "\n"
    return content

parseImage :: Parser Block
parseImage = do
  _ <- match "!"
  _ <- match "["
  alt <- manyUntil (match "]") (satisfy isPlain)
  _ <- match "]"

  _ <- match "("
  src <- manyUntil (match ")") (satisfy isPlain)
  _ <- match ")"

  return $ Image (alt, src)

parseHorizontalBreak :: Parser Block
-- `<$` vs. `<$>`: Here we're using `<$`, which ends up mapping the lhs on 
parseHorizontalBreak = HorizontalBreak <$ match "---"

parseCodeBlock :: Parser Block
parseCodeBlock = do
  _ <- match "```"
  language <- manyUntil (match "\n") (satisfy isPlain)
  _ <- match "\n"
  
  code <- manyUntil (match "\n```") (satisfy $ \_ -> True)

  _ <- match "\n```"
  return $ CodeBlock (language, code)

parseOrderedList :: Parser Block
parseOrderedList = do
  -- Parse identation, +2 is for the period and space.
  spaces <- length <$> many (match " ")
  firstIndent <- (+2) . length <$> some (satisfy isDigit)
  _ <- match "."
  _ <- match " "
  first <- parseItem (spaces + firstIndent)

  rest <- many $ do
    _ <- match "\n"
    _ <- Parser.repeat spaces $ match " "
    marker <- (+2) . length <$> some (satisfy isDigit)
    _ <- match "."
    _ <- match " "
    parseItem (spaces + marker)

  return $ OrderedList (first:rest)

  where 
  parseItem :: Int -> Parser [Block]
  parseItem indent = do
    first <- parseParagraph
    rest <- many $ parseNested indent

    return (first:rest)

parseUnorderedList :: Parser Block 
parseUnorderedList = do
  -- Parse indentation, +2 is for the marker and space.
  indent <- (+2) . length <$> many (match " ")
  marker <- (match "*" <|> match "-")
  _ <- match " "
  first <- parseItem indent

  -- Now parse the rest, based on the first marker.
  rest <- many $ do
    _ <- match "\n"
    _ <- Parser.repeat indent $ match " "
    _ <- match marker
    _ <- match " " 
    parseItem indent

  return $ UnorderedList (first:rest)

  where
  parseItem :: Int -> Parser [Block]
  parseItem indent = do 
    first <- parseParagraph
    rest <- many $ parseNested indent

    return (first:rest)

-- The way most Markdown parsers work when handling nested content inside lists is
-- to take (indentation of whole list + marker width), and make sure that anything 
-- indented >= that, which is kind of insane.
parseNested :: Int -> Parser Block 
parseNested indent = do
  _ <- some $ match "\n"
  _ <- check $ Parser.repeat indent $ match " "
  parseBlock

parseInline :: Parser Inline 
parseInline = parseLink <|> parseCode <|> parseBold <|> parseItalic <|> parsePlain

parsePlain :: Parser Inline
parsePlain = do
  -- Parse at least one character, usually a special character that wasn't able to
  -- be parsed.
  first <- satisfy $ \_ -> True
  rest <- many $ satisfy isPlain

  return $ Plain (first:rest)

parseItalic :: Parser Inline
parseItalic = do
  _ <- match "*"
  nested <- manyUntil (match "*") parseInline
  _ <- match "*"

  return $ Italic nested

parseBold :: Parser Inline
parseBold = do
  _ <- match "**"
  nested <- manyUntil (match "**") parseInline
  _ <- match "**"

  return $ Bold nested

parseCode :: Parser Inline
parseCode = do
  _ <- match "`"
  code <- manyUntil (match "`") (satisfy isPlain)
  _ <- match "`"

  return $ Code code

parseLink :: Parser Inline
parseLink = do
  _ <- match "["
  nested <- manyUntil (match "]") parseInline
  _ <- match "]"

  _ <- match "("
  link <- manyUntil (match ")") (satisfy isPlain)
  _ <- match ")"

  return $ Link (link, nested)

isPlain :: SatisfyFunction
isPlain c =
  c /= '[' &&
  c /= ']' &&
  c /= '>' &&
  c /= '*' &&
  c /= '`' &&
  c /= '\n'

isDigit :: SatisfyFunction
isDigit c = 
  c == '1' ||
  c == '2' ||
  c == '3' ||
  c == '4' || 
  c == '5' ||
  c == '6' ||
  c == '7' ||
  c == '8' ||
  c == '9' ||
  c == '0'
