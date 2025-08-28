{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_md2html (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "md2html"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Markdown \8594 HTML converter (Haskell)"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
