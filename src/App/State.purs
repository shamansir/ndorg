module App.State where

import Prelude


import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Construct (empty) as Org


data Context
    = TopLevel
    | InSection
    | InBlock
    -- TODO | Planning
    | EnteringWords


data PathItem =
    Top


data Path
    = Nowhere
    | Block Int
    | Section Int
    | Property Int
    | Word Int
    | Deeper Path Path


data Focus = Focus Path


-- TODO: undo history


type State =
    { context :: Context
    , focus :: Focus
    , file :: OrgFile
    , chars :: String -- TEMPORARY
    }



init :: State
init =
    { context : TopLevel
    , focus : Focus Nowhere
    , file : Org.empty
    , chars : ""
    }