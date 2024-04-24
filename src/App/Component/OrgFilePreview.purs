module App.Component.OrgFilePreview where

import Prelude

import Type.Proxy (Proxy(..))

import Halogen as H

_preview = Proxy :: _ "org-preview"


type Slot = H.Slot Query Output


data Query a = Query a


type Output = Unit