{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.List (isPrefixOf)

-- Entry: md2html input.md output.html
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inp, out] -> do
      md <- readFile inp
      let htmlBody = unlines (map blockToHtml (blocks (lines md)))
      let page = wrapHtml htmlBody
      writeFile out page
      putStrLn ("Wrote " ++ out)
    _ -> putStrLn "Usage: md2html <input.md> <output.html>"

wrapHtml :: String -> String
wrapHtml body =
  "<!doctype html>\n<html><head><meta charset=\"utf-8\"><title>MD &rarr; HTML</title></head><body>\n"
  ++ body ++ "\n</body></html>\n"

-- Split lines into blocks separated by blank lines
blocks :: [String] -> [Block]
blocks = go [] []
  where
    go acc cur [] =
      reverse (finish acc cur)
    go acc cur (l:ls)
      | null (trim l) = go (finish acc cur) [] ls
      | otherwise     = go acc (cur ++ [l]) ls

    finish :: [Block] -> [String] -> [Block]
    finish acc []  = acc
    finish acc cur =
      case trim (head cur) of
        l | "# "   `isPrefixOf` l -> H 1 (drop 2 l) : acc
          | "## "  `isPrefixOf` l -> H 2 (drop 3 l) : acc
          | "### " `isPrefixOf` l -> H 3 (drop 4 l) : acc
          | otherwise             -> Para (unwords cur) : acc



data Block = H Int String | Para String

blockToHtml :: Block -> String
blockToHtml (H n s)   = "<h"++show n++">" ++ inline s ++ "</h"++show n++">"
blockToHtml (Para s)  = "<p>" ++ inline s ++ "</p>"

-- Very tiny inline parser: **bold** and *italic*
inline :: String -> String
inline = go
  where
    go "" = ""
    go s
      | "**" `isPrefixOf` s =
          let (inside, rest) = breakOn "**" (drop 2 s)
          in "<strong>" ++ escape inside ++ "</strong>" ++ go (dropPrefix "**" rest)
      | "*" `isPrefixOf` s =
          let (inside, rest) = breakOn "*" (drop 1 s)
          in "<em>" ++ escape inside ++ "</em>" ++ go (dropPrefix "*" rest)
      | otherwise = head s : go (tail s)

-- Find the next occurrence of a delimiter; if not found, treat all as text
breakOn :: String -> String -> (String, String)
breakOn delim s = case splitOnce delim s of
  Nothing        -> (s, "")
  Just (a, rest) -> (a, rest)

splitOnce :: String -> String -> Maybe (String, String)
splitOnce delim = go ""
  where
    go acc "" = Nothing
    go acc r
      | delim `isPrefixOf` r = Just (reverse acc, r)
      | otherwise = go (head r : acc) (tail r)

dropPrefix :: String -> String -> String
dropPrefix pfx s
  | pfx `isPrefixOf` s = drop (length pfx) s
  | otherwise          = s

trim :: String -> String
trim = f . f where f = reverse . dropWhile (`elem` [' ', '\t', '\r'])

-- Minimal HTML escape (only what we used)
escape :: String -> String
escape = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc c   = [c]
