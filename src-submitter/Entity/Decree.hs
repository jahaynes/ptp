module Entity.Decree where

import Entity.Node (Node)

data Decree = ValueDecree !String
            | ElectLeader !Node
                deriving (Read, Show)