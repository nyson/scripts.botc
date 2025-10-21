module PrettyPrint where

class PrettyPrint a where
    pretty :: a -> String