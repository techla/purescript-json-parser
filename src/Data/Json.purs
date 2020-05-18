module Json where

import Prelude (class Eq, class Show, (<>), show, (<$>))
import Data.Array (fold)
import Data.Tuple (Tuple(..))

data Json
  = JsonNull
  | JsonBool Boolean
  | JsonNumber Number
  | JsonString String
  | JsonArray (Array Json)
  | JsonObject (Array (Tuple String Json))

instance jsonShow :: Show Json where
  show json = case json of
    JsonNull -> "JsonNull"
    JsonBool b -> "JsonBool(" <> show b <> ")"
    JsonNumber n -> "JsonNumber(" <> show n <> ")"
    JsonString s -> "JsonString(" <> s <> ")"
    JsonArray a -> "JsonArray(" <> fold (show <$> a) <> ")"
    JsonObject (o) -> "JsonObject([" <> fold ((\(Tuple k v) -> "(" <> show k <> ", " <> show v <> ")") <$> o) <> "])"

derive instance jsonEq :: Eq Json
