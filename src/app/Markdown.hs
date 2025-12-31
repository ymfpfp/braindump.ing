module Markdown where

import Control.Applicative (Alternative(..))

type Document = [Block]

data Block
  = Paragraph [Inline]
  | Heading (Int, [Inline])
  | Blockquote [Block]
  | OrderedList [Block]
  | UnorderedList [Block]
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
  OrderedList items -> wrap "ol" $ concatMap (wrap "li" . blockToHtml) items
  UnorderedList items -> wrap "ul" $ concatMap (wrap "li" . blockToHtml) items
  Image (alt, src) -> wrapWithProps "img" [("alt", alt), ("src", src)] ""
  HorizontalBreak -> "<hr>"
  CodeBlock (language, code) -> 
    wrap "pre" $ wrapWithProps "code" [("class", "language-" ++ language)] code

inlineToHtml :: Inline -> String
inlineToHtml inline = case inline of
  Plain s -> s
  Italic nested -> wrap "i" $ concatMap inlineToHtml nested
  Bold nested -> wrap "b" $ concatMap inlineToHtml nested
  Code code -> wrap "code" code
  Link (link, nested) -> 
    wrapWithProps "a" [("href", link)] $ concatMap inlineToHtml nested

-- `newtype` has the shape of a record with one key. This is useful for defining
-- a new type that wraps an existing type with one value constructor, so that here
-- we can implement `Parser` as an instance of a couple of type classes below.
newtype Parser t = Parser { runParser :: String -> Maybe (t, String) }

-- Functors are types that can have a function be mapped over them. The common example
-- is `Maybe`, where you can run a function on the value it's wrapping. Here, we 
-- return a `Maybe(t, String)`, so we can `fmap` over it so it becomes 
-- `Maybe (f t, String)`.
--
-- It is helpful to think of functors as "a context around a type".
instance Functor Parser where 
  fmap f parser = Parser (\s ->
    case (runParser parser) s of
      Nothing -> Nothing
      Just (inside, stream) -> Just (f inside, stream))

-- Applicative functors are an extra addition to functors that lets us map the 
-- function inside one functor to another functor, thus why it's called "applicative".
-- It's like chaining two values together by applying the function in one to another.
instance Applicative Parser where
  -- A parser that takes a value and returns it, but wrapped in the `Parser` context.
  pure x = Parser (\s -> Just (x, s))

  -- Apply, e.g. `Just (+3) <*> Just 4` will apply the function inside `Just` on lhs
  -- to the inner value inside `Just` on rhs.
  --
  -- `*>` and `<*` will be implemented from this, and these are more like the
  -- traditional ideas of chaining. This is very hard to wrap my head around, 
  -- but let's say:
  --
  -- `Just 3 <* Just 5` becomes
  -- `(\a _ -> a) <$> (Just 3) <*> (Just 5)`
  -- And `<$>` has higher precedence, so we wrap `Just 3` in the function, i.e.
  -- `\3 _ -> 3`. Then we can pass that in to `<*>`, which maps the function over 
  -- `Just 5` to produce `Just 3`. Thus we are able to execute both functions and 
  -- return the result of the first which is pretty cool.
  --
  -- The difference between `*>` and `<*` and monadic chaining is that applicative 
  -- chaining means you have no way to change the "shape" of the resulting
  --
  -- This is based off monad chaining for `Maybe` type, i.e.
  pf <*> px = Parser (\stream -> do
    -- `pf` is a parser that returns a function.
    (f, fs) <- (runParser pf) stream
    (x, xs) <- (runParser px) fs
    Just (f x, xs))

-- And now we get to monads!
instance Monad Parser where
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  parser >>= f = Parser (\stream ->
    case (runParser parser) stream of
      Nothing -> Nothing
      Just (x, xs) -> (runParser $ f x) xs)

-- Parser combinators wrap around other parser(s) to manipulate the results.
instance Alternative Parser where
  -- A parser that takes a value and returns `Nothing`.
  empty = Parser (\_ -> Nothing)

  -- A parser combinator that performs || on the results of two parsers.
  px <|> py = Parser (\stream ->
    case (runParser px) stream of
      Nothing -> (runParser py) stream
      Just pz -> Just pz)

  -- A parser combinator that turns a parser that parses 1 thing into one that
  -- parses 1+, always returning a list.
  many :: Parser t -> Parser [t]
  many parser = Parser (\stream ->
    case (runParser parser) stream of
      Nothing -> Just([], stream)
      Just (x, xs) -> 
        case (runParser $ many parser) xs of
          -- This is an unreachable case.
          Nothing -> Nothing
          Just (y, ys) -> Just (x:y, ys))

  some parser = Parser (\stream ->
    case (runParser parser) stream of
      Nothing -> Nothing
      Just (x, xs) ->
        case (runParser $ many parser) xs of
          -- This is an unreachable case.
          Nothing -> Nothing
          Just (y, ys) -> Just (x:y, ys))

-- A couple more custom parser combinators.
type SatisfyFunction = Char -> Bool
satisfy :: SatisfyFunction -> Parser Char
satisfy f = Parser (\stream -> 
  case stream of
    [] -> Nothing
    (x:xs) ->
      if (f x)
        then Just (x, xs)
        else Nothing)

match :: String -> Parser String
match [] = pure []
-- We want to `fmap (:)` over the match so we can prepend it to `match`, which means
-- this is recursive.
-- TODO: Explain this??
match (x:xs) = (:) <$> satisfy (\c -> c == x) <*> match xs

-- Handle optional parsing by wrapping values in `Maybe`.
optional :: Parser t -> Parser (Maybe t)
optional parser = (Just <$> parser) <|> pure Nothing

-- Ignore `a`, type that doesn't affect the output.
manyUntil :: Parser a -> Parser t -> Parser [t]
manyUntil condition parser = Parser (\stream ->
  case (runParser condition) stream of
    -- If we hit it, return an empty list.
    Just _ -> Just([], stream)
    Nothing -> case (runParser parser) stream of
      Nothing -> Nothing
      Just (x, xs) -> case (runParser $ manyUntil condition parser) xs of
        Nothing -> Nothing
        Just (y, ys) -> Just (x:y, ys))

-- The actual parsers. 
parse :: String -> Document 
-- Either return `[]` or the `[Block]` part of the final `Just (blocks, stream)`.
parse raw = maybe [] fst (runParser (many parseBlock) raw)

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
-- Anyways that's all monads are really about. They're a special trait for functors
-- that allows us to chain together a bunch of similar functors in such a way.
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
  lines <- unlines <$> some parseLines
  case (runParser $ many parseBlock) lines of
    Nothing -> empty
    Just (blocks, _) -> return $ Blockquote blocks

  where 
  parseLines :: Parser String
  parseLines = do
    _ <- match ">"
    _ <- optional $ match " "
    content <- manyUntil (match "\n") (satisfy $ \c -> c /= '\n')
    _ <- optional $ match "\n"
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
parseOrderedList = OrderedList <$> some item
  where 
  item = do
    _ <- some $ satisfy isDigit
    _ <- match "."
    _ <- match " "
    contents <- parseParagraph
    _ <- optional $ match "\n"
    return contents

-- TODO: This isn't working for asterisks for some reason.
parseUnorderedList :: Parser Block
parseUnorderedList = do
  marker <- (match "*" <|> match "-")
  _ <- match " "
  first <- parseParagraph
  
  -- Now parse the rest, based on the first marker.
  rest <- some $ do
    _ <- match "\n"
    _ <- match marker
    parseParagraph

  return $ UnorderedList (first:rest)

parseInline :: Parser Inline 
parseInline = parseLink <|> parseCode <|> parseBold <|> parseItalic <|> parsePlain

parsePlain :: Parser Inline
parsePlain = Plain <$> (some $ satisfy isPlain)

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

-- These are all special potentially inline characters.
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
