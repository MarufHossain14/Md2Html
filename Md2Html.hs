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
      let htmlBody = unlines (map mdLineToHtml (lines md))
      let page = wrapHtml htmlBody
      writeFile out page
      putStrLn ("Wrote " ++ out)
    _ -> putStrLn "Usage: md2html <input.md> <output.html>"

wrapHtml :: String -> String
wrapHtml body =
  "<!doctype html>\n<html><head><meta charset=\"utf-8\"><title>MD &rarr; HTML</title></head><body>\n"
  ++ body ++ "\n</body></html>\n"


mdLineToHtml :: String -> String
mdLineToHtml line
  | null (trim line)     = ""  -- skip blank lines
  | "# "   `isPrefixOf` line = "<h1>" ++ inline (drop 2 line) ++ "</h1>"
  | "## "  `isPrefixOf` line = "<h2>" ++ inline (drop 3 line) ++ "</h2>"
  | "### " `isPrefixOf` line = "<h3>" ++ inline (drop 4 line) ++ "</h3>"
  | otherwise             = "<p>"  ++ inline line ++ "</p>"

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
trim = f . f where f = reverse . dropWhile (`elem` [' ', '\t'])

-- Minimal HTML escape (only what we used)
escape :: String -> String
escape = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc c   = [c]
