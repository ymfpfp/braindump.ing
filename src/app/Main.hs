module Main where

import Control.Monad (when)
import Data.List (isSuffixOf, sortBy)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Parser
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesDirectoryExist,
  listDirectory)
import System.Process (readProcess)

import qualified Markdown as Md
import qualified Extensions as Extensions
import Utils (capitalize, slug, trim)

type Transformation = String -> IO (String, String)
type Transformations = Map String Transformation

-- This is really bad. TODO
replace :: Map String String -> Parser String 
replace args = Parser (\stream ->
  case stream of
    "" -> Just ([], stream)
    (x:xs) -> if (x == '{')
      then
        case (runParser $ manyUntil (match "}") (satisfy (\c -> c /= '}' && c /= '\n'))) xs of
          Nothing -> case (runParser $ replace args) xs of
            Nothing -> Nothing
            Just (y, ys) -> Just (x:y, ys)
          Just (arg, ys) -> case Map.lookup arg args of
            Nothing -> Nothing
            Just new -> case (runParser $ match "}" *> replace args) ys of
              Nothing -> Nothing
              Just (z, zs) -> Just (new ++ z, zs)
      -- Otherwise, we should just continue with the rest of the stream.
      else
        case (runParser $ replace args) xs of
          -- This is an unreachable case.
          Nothing -> Nothing
          Just (y, ys) -> Just (x:y, ys))

inject :: String -> Map String String -> String
inject template args = maybe "" fst ((runParser $ replace args) template)

main :: IO ()
main = do
  createDirectoryIfMissing True outputDirectory

  -- Output all assets.
  outputDirectoryTo assetsDirectory outputDirectory [".html", ".ts"] False

  -- Now begin to process the actual Markdown content.
  outputDirectoryTo inputDirectory outputDirectory [] True

  where
  outputDirectoryTo :: String -> String -> [String] -> Bool -> IO ()
  outputDirectoryTo input output blacklist list = do
    entries <- listDirectory input
    -- mapM_ :: Monad m -> (a -> m ()) -> [a] -> m ()
    -- mapM_ :: Monad IO -> (String -> IO ()) -> [String] -> IO ()
    mapM_ (\entry -> do
      let path = input ++ entry
      let outputPath = output ++ entry
      isDir <- doesDirectoryExist path
      case isDir of
        True -> do
          createDirectoryIfMissing True outputPath
          outputDirectoryTo (path ++ "/") (outputPath ++ "/") blacklist list
          when list (generateList path outputPath)
        False -> when (not $ any (\suffix -> suffix `isSuffixOf` entry) blacklist) $ do
          -- Check if there's any transformations that need to happen.
          case Map.lookup (extension entry) transformations of
            Nothing -> copyFile path outputPath
            Just f -> do
              (transformedPath, transformed) <- f path
              writeFile (output ++ transformedPath) transformed) entries

  -- The first item will contain the character being split on.
  breakFromEnd :: (t -> Bool) -> [t] -> ([t], [t])
  breakFromEnd split s = let (suffix, prefix) = break split (reverse s)
                         in (reverse prefix, reverse suffix)

  filename :: String -> String
  filename s = snd $ breakFromEnd (== '/') $ fst $ breakFromEnd (== '.') s

  extension :: String -> String
  extension s = snd $ breakFromEnd (== '.') s

  assetsDirectory = "../include/"
  layoutDirectory = "../layout/"
  inputDirectory = "../routes/"
  outputDirectory = "../out/"

  generateList :: String -> String -> IO ()
  generateList directory _ = do
    entries <- listDirectory directory

    wrapperTemplate <- readFile $ layoutDirectory ++ "wrapper.html"
    listTemplate <- readFile $ layoutDirectory ++ "list.html"

    posts <- mapM toLink entries
    let sorted = sortBy (\(a, _) (b, _) -> compare b a) posts
    let content = Md.documentToHtml $ map snd sorted

    let inner = inject listTemplate $ Map.fromList [("title", title), ("content", content)]
    let args = Map.fromList
          [("content", inner)
          ,("title", title)
          ,("slug", name ++ ".html")
          ,("description", "Collection of " ++ name)
          ,("toc", "")
          ]
    writeFile (outputDirectory ++ name ++ ".html") (inject wrapperTemplate args)

    where
    -- The last path component, e.g. "../routes/writing" -> "writing".
    name = slug $ snd $ breakFromEnd (== '/') directory
    title = capitalize name

    toLink :: String -> IO (String, Md.Block)
    toLink entry = do
      raw <- readFile $ directory ++ "/" ++ entry
      let (_, fields) = Extensions.frontmatter (Md.parse raw)
      let postTitle = fromMaybe entry $ Map.lookup "title" fields
      let date = fromMaybe "" $ Map.lookup "date" fields
      let href = "/" ++ name ++ "/" ++ filename entry ++ "html"
      return (date, Md.Paragraph [Md.Italic [Md.Plain date], Md.Plain " ", Md.Link (href, [Md.Plain postTitle])])

  -- We wrap both the transformed path and the transformed output in an IO monad!
  transformations :: Transformations 
  transformations = Map.fromList $
    [("scss", \path -> do
        transformed <- readProcess "sass" [path] ""
        return ((filename path) ++ "css", transformed))
    ,("md", transformMarkdown)]

  transformMarkdown :: Transformation
  transformMarkdown = \path -> do
    let transformedPath = (filename path) ++ "html"

    raw <- readFile path
    let (parsed, injections) = Md.parseWithExtensions raw [Extensions.desc, Extensions.parseFrontmatter, Extensions.parseTOC, Extensions.parseFootnotes]
    let layoutPath = trim (== '\n') <$> (Md.documentToText <$> Map.lookup "layout" injections)

    -- Reading the file is REALLY bad every time. TODO
    wrapperTemplate <- readFile $ layoutDirectory ++ "wrapper.html"
    layoutTemplate <- readFile $ layoutDirectory ++ (fromMaybe "default" layoutPath) ++ ".html"

    let injectionsToHtml = Map.map Md.documentToHtml injections
    let args = Map.union injectionsToHtml $ Map.fromList [("content", Md.documentToHtml parsed), ("slug", transformedPath)] 

    -- Inject into HTML template.
    let content = inject layoutTemplate args 

    return (transformedPath, inject wrapperTemplate $ Map.union (Map.fromList [("content", content)]) args)
