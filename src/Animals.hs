
module Animals where

class Parrot a where
  say :: a -> String

instance Parrot Bool where
  say True = "Yes"
  say False = "No"

instance Parrot Char where
  say a = [a]

instance (Parrot a) => Parrot (Maybe a) where
--  say Nothing :: Maybe Bool = ""
--  say Nothing :: Maybe Char = ""
  say Nothing = ""
  say (Just a) = say a