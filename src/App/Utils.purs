module App.Utils where

import Prelude


import Data.Newtype (wrap) as NT
import Web.HTML.Common (ClassName)


cn :: String -> ClassName
cn = NT.wrap