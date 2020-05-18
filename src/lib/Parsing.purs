module Parsing where

import Parser
import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Control.Apply (lift3)
import Data.Array (uncons, span, many, (:))
import Data.Char.Unicode (isDigit, isSpace)
import Data.Eq ((/=))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.String.Common (null)
import Data.String.Read (readDefault)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Global (readFloat)
import Heterogeneous.Mapping (hmap)
import Json (Json(..))
import Prelude (map, pure, ($), (*>), (<$>), (<*), (<*>), (<<<), (==))

charP :: Char -> Parser Char
charP c =
  wrap
    $ \input -> case (uncons (toCharArray input)) of
        Just { head: x, tail: xs }
          | x == c -> Just $ Tuple (fromCharArray xs) c
        _ -> Nothing

stringP :: String -> Parser String
stringP = map fromCharArray <<< sequence <<< map charP <<< toCharArray

spanP :: (Char -> Boolean) -> Parser String
spanP p =
  wrap
    $ \input ->
        let
          { rest, init } = hmap fromCharArray <<< span p <<< toCharArray $ input
        in
          Just $ Tuple rest init

jsonNull :: Parser Json
jsonNull = (\_ -> JsonNull) <$> (stringP "null")

jsonBool :: Parser Json
jsonBool = JsonBool <<< readDefault <$> ((stringP "true") <|> (stringP "false"))

checkNull :: Parser String -> Parser String
checkNull p =
  wrap
    $ \input -> case (unwrap p input) of
        Just (Tuple rest parsed) -> if (null parsed) then Nothing else Just $ Tuple rest parsed
        Nothing -> Nothing

jsonNumber :: Parser Json
jsonNumber =
  JsonNumber <<< readFloat
    <$> checkNull (spanP isDigit)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP ((/=) '"') <* charP '"'

jsonString :: Parser Json
jsonString = JsonString <$> stringLiteral

sepBy :: forall a b. Parser a -> Parser b -> Parser (Array b)
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

whiteSpace :: Parser String
whiteSpace = spanP isSpace

jsonArray :: Parser Json
jsonArray = JsonArray <$> (charP '[' *> whiteSpace *> elements <* whiteSpace <* charP ']')
  where
  elements = defer (\_ -> sepBy (whiteSpace *> charP ',' <* whiteSpace) $ jsonValue)

jsonObject :: Parser Json
jsonObject =
  JsonObject
    <$> (charP '{' *> whiteSpace *> sepBy (whiteSpace *> charP ',' <* whiteSpace) pair <* whiteSpace <* charP '}')
  where
  pair =
    defer
      ( \_ ->
          lift3 (\key _ value -> Tuple key value)
            stringLiteral
            (whiteSpace *> charP ':' <* whiteSpace)
            jsonValue
      )

jsonValue :: Parser Json
jsonValue = defer (\_ -> jsonNull <|> jsonBool <|> jsonString <|> jsonArray <|> jsonObject <|> jsonNumber)
