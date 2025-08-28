module Renderer (renderPage, blockToHtml) where
import AST
renderPage :: String -> String
renderPage body = "<!doctype html><html><body>" ++ body ++ "</body></html>"
blockToHtml :: Block -> String
blockToHtml (H n s)  = "<h"++show n++">" ++ s ++ "</h"++show n++">"
blockToHtml (Para s) = "<p>" ++ s ++ "</p>"
