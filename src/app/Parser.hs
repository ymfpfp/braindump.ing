module Parser where

import Control.Applicative (Alternative(..))

-- `newtype` has the shape of a record with one key. This is useful for defining
-- a new type that wraps an existing type with one value constructor, so that here
-- we can implement `Parser` as an instance of a couple of type classes below.
newtype Parser t = Parser { runParser :: String -> Maybe (t, String) }

-- Functors are types that serve as a context around values, allowing functions to be
-- mapped over the value inside. The common example is `Maybe`, where you can run a 
-- function on the value it's wrapping. Here, we return a `Maybe (t, String)`, so we
-- can `fmap` over it so it becomes `Maybe (f t, String)`.
--
-- The infix operator for this is `<$>`.
instance Functor Parser where
  fmap apply parser = Parser (\s ->
    case (runParser parser) s of
      Nothing -> Nothing
      Just (inside, stream) -> Just (apply inside, stream))

-- Applicative functors are an extra addition to functors that lets us map the 
-- function inside one functor to another functor, thus why it's called "applicative".
-- It's like chaining two values together by applying the function in one to another.
instance Applicative Parser where
  -- A parser that takes a value and returns it, but wrapped in the `Parser` context.
  pure x = Parser (\stream -> Just (x, stream))

  -- Apply, e.g. `Just (+3) <*> Just 4` will apply the function inside `Just` on lhs
  -- to the inner value inside `Just` on rhs.
  --
  -- `*>` and `<*` will be implemented from this, and these are implemented
  -- automatically with `<$>` and `<*>` for primitive chaining that ignores the 
  -- context outside, e.g.
  --
  -- `Just 3 <* Just 5` becomes
  -- `(\a _ -> a) <$> (Just 3) <*> (Just 5)`, as Haskell implements `*>` as
  -- `(\a _ -> a) <$> a <*> b`. 
  --
  -- Thus we are able to execute both functions and return the result of the first 
  -- which is pretty cool.
  --
  -- The difference between `*>` and `<*` and monadic chaining is that applicative
  -- chaining means you have no way to change the "shape" of the result.
  -- 
  -- This is based off monad chaining for `Maybe` type, that is, we are able
  pf <*> px = Parser (\stream -> do
    -- `pf` is a parser that returns a function.
    (f, fs) <- (runParser pf) stream
    (x, xs) <- (runParser px) fs
    Just (f x, xs))

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
  -- parses 0+, always returning a list.
  many :: Parser t -> Parser [t]
  many parser = Parser (\stream ->
    case (runParser parser) stream of
      Nothing -> Just ([], stream)
      Just (x, xs) ->
        case (runParser $ many parser) xs of
          -- This is an unreachable case.
          Nothing -> Nothing
          Just (y, ys) -> Just (x:y, ys))

  -- 1+.
  some parser = Parser (\stream ->
    case (runParser parser) stream of
      Nothing -> Nothing
      Just (x, xs) ->
        case (runParser $ many parser) xs of
        -- This is an unreachable case.
        Nothing -> Nothing
        Just (y, ys) -> Just (x:y, ys))

-- A couple more custom parser combinators.
-- Look ahead, but don't alter the stream.
check :: Parser t -> Parser t
check parser = Parser (\stream -> 
  case (runParser parser) stream of
    Nothing -> Nothing
    Just (x, _) -> Just (x, stream))

type SatisfyFunction = Char -> Bool
satisfy :: SatisfyFunction -> Parser Char
satisfy f = Parser (\stream -> 
  case stream of
    [] -> Nothing
    (x:xs) ->
      if (f x)
        then Just (x, xs)
        else Nothing)

-- Ignore `a`, type that doesn't affect the output.
manyUntil :: Parser a -> Parser t -> Parser [t]
manyUntil condition parser = Parser (\stream ->
  case (runParser condition) stream of
    -- If we hit it, return an empty list.
    Just _ -> Just ([], stream)
    Nothing -> case (runParser parser) stream of
      Nothing -> Nothing
      Just (x, xs) -> case (runParser $ manyUntil condition parser) xs of
        Nothing -> Nothing
        Just (y, ys) -> Just (x:y, ys))

-- Handle optional parsing by wrapping values in `Maybe`.
optional :: Parser t -> Parser (Maybe t)
optional parser = (Just <$> parser) <|> pure Nothing

match :: String -> Parser String
match [] = pure []
-- We want to `fmap (:)` over the match so we can prepend it to `match`, which means
-- this is recursive.
--
-- As for `<*>`, we take the function in the context on the lhs, and apply it to the
-- value in the context on the rhs.
match (x:xs) = (:) <$> satisfy (\c -> c == x) <*> match xs

-- Repeat a parser combinator, failing if unable to repeat it successfully.
repeat :: Int -> Parser t -> Parser [t]
repeat 0 _ = pure []
repeat n parser = Parser (\stream ->
  case (runParser parser) stream of
    Nothing -> Nothing
    Just (x, xs) -> case (runParser $ Parser.repeat (n - 1) parser) xs of
      Nothing -> Nothing
      Just (y, ys) -> Just (x:y, ys))
