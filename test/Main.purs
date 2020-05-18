module Main where

import Prelude (Unit, discard, ($))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Json (Json(..))
import Parsing (jsonValue)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Json parser on null" do
          it "Should parse to JsonNull" do
            (unwrap jsonValue $ "nullrest") `shouldEqual` (Just $ Tuple "rest" JsonNull)
        describe "Json parser on booleans" do
          it "Should parse to JsonBool(true)" do
            (unwrap jsonValue $ "truerest") `shouldEqual` (Just $ Tuple "rest" (JsonBool true))
          it "Should parse to JsonBool(false)" do
            (unwrap jsonValue $ "falserest") `shouldEqual` (Just $ Tuple "rest" (JsonBool false))
        describe "Json parser on strings" do
          it "Should parse strings" do
            (unwrap jsonValue $ "\"hello world\"rest") `shouldEqual` (Just $ Tuple "rest" (JsonString "hello world"))
          it "Should parse the empty string" do
            (unwrap jsonValue $ "\"\"") `shouldEqual` (Just $ Tuple "" (JsonString ""))
          it "Should parse only spaced strings" do
            (unwrap jsonValue $ "\"    \"") `shouldEqual` (Just $ Tuple "" (JsonString "    "))
        describe "Json parser on integers" do
          it "Should parse to JsonNumber" do
            (unwrap jsonValue $ "5rest") `shouldEqual` (Just $ Tuple "rest" (JsonNumber 5.0))
        describe "Json parser on arrays" do
          it "Should parse to JsonArray" do
            (unwrap jsonValue $ "[\"hello world\", 5]rest") `shouldEqual` (Just $ Tuple "rest" (JsonArray ([ JsonString ("hello world"), JsonNumber (5.0) ])))
        describe "Json parser on object" do
          it "Should parse to JsonObject" do
            (unwrap jsonValue $ "{\"hello\": \"world\", \"test\": [1]}rest")
              `shouldEqual`
                ( Just
                    $ Tuple "rest"
                        ( JsonObject
                            ( [ Tuple "hello" (JsonString "world")
                              , Tuple "test" (JsonArray ([ JsonNumber 1.0 ]))
                              ]
                            )
                        )
                )
        describe "Json parser on random input" do
          it "Should return Nothing" do
            (unwrap jsonValue $ "random") `shouldEqual` Nothing
