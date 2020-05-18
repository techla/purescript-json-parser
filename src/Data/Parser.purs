module Parser where

import Control.Lazy (class Lazy)
import Control.Alternative (class Alt, class Plus, (<|>), class Alternative)
import Data.Newtype (class Newtype, unwrap)
import Prelude (class Applicative, class Apply, class Functor, bind, ($), unit)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype Parser a
  = Parser (String -> (Maybe (Tuple String a)))

derive instance parserNewtype :: Newtype (Parser a) _

derive instance parserFunctor :: Functor Parser

instance parserApply :: Apply Parser where
  apply p1 p2 =
    Parser
      $ \input -> do
          Tuple input' f <- (unwrap p1) input
          Tuple input'' a <- (unwrap p2) input'
          Just (Tuple input'' (f a))

instance parserApplicative :: Applicative Parser where
  pure x = Parser $ \input -> Just (Tuple input x)

instance parserAlt :: Alt Parser where
  alt p1 p2 = Parser $ \input -> ((unwrap p1) input) <|> ((unwrap p2) input)

instance parserPlus :: Plus Parser where
  empty = Parser $ \_ -> Nothing

instance parserAlternative :: Alternative Parser

instance parserLazy :: Lazy (Parser a) where
  defer f = Parser \input -> unwrap (f unit) input
