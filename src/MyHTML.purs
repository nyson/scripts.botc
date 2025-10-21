module MyHTML where

import Role (Role)

type Model = { pages :: Array (Array Role)}

data Msg
    = Nop
