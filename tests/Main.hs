{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import qualified Json.Decode as Decode
import Cherry.Prelude (Result(..))


main :: IO ()
main = hspec $ do
  describe "Cherry.Json.Decode" $ do
    it "can decode a string" $
      let bytestring = Text.encodeUtf8 "\"string\""
          decoder = Decode.string
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok "string"

    it "can decode an int" $
      let bytestring = Text.encodeUtf8 "12"
          decoder = Decode.int
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok 12

    it "can decode a negative int" $
      let bytestring = Text.encodeUtf8 "-4"
          decoder = Decode.int
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok (negate 4)

    it "can decode a large int" $
      let bytestring = Text.encodeUtf8 "1801439850948"
          decoder = Decode.int
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok 1801439850948

    it "can decode a negative large int" $
      let bytestring = Text.encodeUtf8 "-1801439850948"
          decoder = Decode.int
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok (negate 1801439850948)

    it "can decode an exponent" $
      let bytestring = Text.encodeUtf8 "2e8"
          decoder = Decode.float
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok 2.0e8

    it "can decode a whole float" $
      let bytestring = Text.encodeUtf8 "0.4"
          decoder = Decode.float
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok 0.4

    it "can decode a negative whole float" $
      let bytestring = Text.encodeUtf8 "-0.4"
          decoder = Decode.float
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok (negate 0.4)

    it "can decode a float" $
      let bytestring = Text.encodeUtf8 "4.2"
          decoder = Decode.float
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok 4.2

    it "can decode a negative float" $
      let bytestring = Text.encodeUtf8 "-4.2"
          decoder = Decode.float
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok (negate 4.2)

    it "can decode a large float" $
      let bytestring = Text.encodeUtf8 "0.1274960773527468635486"
          decoder = Decode.float
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok 0.12749607735274687

    it "can decode true" $
      let bytestring = Text.encodeUtf8 "true"
          decoder = Decode.bool
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok True

    it "can decode false" $
      let bytestring = Text.encodeUtf8 "false"
          decoder = Decode.bool
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok False

    it "can decode null" $
      let bytestring = Text.encodeUtf8 "null"
          decoder = Decode.null 0
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok (0 :: Int)

    it "can decode a tuple" $
      let bytestring = Text.encodeUtf8 "[1, false]"
          decoder = Decode.pair Decode.int Decode.bool
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok (1, False)

    it "can decode an object" $
      let bytestring = Text.encodeUtf8 "{ \"property\": 13 }"
          decoder = Decode.field "property" Decode.int
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok 13

    it "can decode an object with several properties" $
      let bytestring = Text.encodeUtf8 "{ \"name\": \"Tereza\", \"age\": 24 }"
          decoder =
            Decode.map2 (,)
              (Decode.field "name" Decode.string)
              (Decode.field "age" Decode.int)
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok ( "Tereza", 24 )


    it "can decode a dict" $
      let bytestring = Text.encodeUtf8 "{ \"a\": \"1\", \"b\": \"2\" }"
          decoder = Decode.dict Decode.string
      in
      Decode.fromByteString decoder bytestring `shouldBe` Ok (Map.fromList [ ( "a", "1" ), ( "b", "2" ) ])
