module App.Action where

import Prelude


import Halogen as H
import Web.UIEvent.KeyboardEvent as KE

import App.Keys (Combo(..))


data Action
  = Initialize
  | None
  | Increment
  | HandleKeyDown H.SubscriptionId KE.KeyboardEvent
  | HandleKeyUp H.SubscriptionId KE.KeyboardEvent
  | WaitForCombos (Combo Action) (Array (Combo Action))
  | ClearCombo
  | AddHeading
  | AddProperty