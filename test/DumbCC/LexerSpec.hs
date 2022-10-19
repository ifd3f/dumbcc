module DumbCC.LexerSpec where

import Control.Monad.State
import Data.Traversable
import DumbCC.Lexer
import qualified DumbCC.Lexer as L
import Test.Hspec

spec :: Spec
spec = do
  describe "takeRegex" $ do
    it "only scans at the head" $ do
      let result = runState (takeRegex "\\d+") (0, "a12")
      result `shouldBe` ("", (0, "a12"))

  describe "takeChar" $ do
    it "does nothing with empty read head" $ do
      let result = runState takeChar (10, "")
      result `shouldBe` (Nothing, (10, ""))

    it "advances read head with char" $ do
      let result = runState takeChar (10, "abc")
      result `shouldBe` (Just 'a', (11, "bc"))

  describe "takeId" $ do
    it "does nothing with empty read head" $ do
      let result = runState takeId (10, "")
      result `shouldBe` (Nothing, (10, ""))

    it "consumes id until non-id char" $ do
      let result = runState takeId (10, "aBc|def")
      result `shouldBe` (Just "aBc", (13, "|def"))

    it "returns nothing if no id at head" $ do
      let result = runState takeId (10, ",def")
      result `shouldBe` (Nothing, (10, ",def"))

  describe "takeId" $ do
    it "does nothing with empty read head" $ do
      let result = runState takeId (10, "")
      result `shouldBe` (Nothing, (10, ""))

    it "consumes id until non-id char" $ do
      let result = runState takeId (10, "aBc|def")
      result `shouldBe` (Just "aBc", (13, "|def"))

    it "returns nothing if no id at head" $ do
      let result = runState takeId (10, ",def")
      result `shouldBe` (Nothing, (10, ",def"))

  describe "takeFloat" $ do
    it "rejects bare +" $ do
      let result = runState takeFloat (0, "+")
      result `shouldBe` (Nothing, (0, "+"))

    it "rejects bare f" $ do
      let result = runState takeFloat (0, "f")
      result `shouldBe` (Nothing, (0, "f"))

    _ <- for
      [ "3232",
        "-82",
        "+832",
        "18.23",
        ".1232",
        "-.1232",
        "732.32f",
        "-83.23",
        "-83.23f",
        "+832.3f"
      ]
      $ \x -> do
        it ("takes " ++ show x) $ do
          let result = runState takeFloat (0, x)
          result `shouldBe` (Just x, (length x, ""))

    pure ()

  describe "lex" $ do
    _ <- for
      [ ( " if (3+ 2 == 5)",
          [ TId "if",
            TPnc LParen,
            TNum "3",
            TPnc Add,
            TNum "2",
            TPnc Eq,
            TNum "5",
            TPnc RParen
          ]
        ),
        ( "three = five * 32;",
          [ TId "three",
            TPnc Asgn,
            TId "five",
            TPnc Mul,
            TNum "32",
            TPnc Semicolon
          ]
        ),
        ( "12 {(foo 32 !) bar}",
          [ TNum "12",
            TPnc LCurl,
            TPnc LParen,
            TId "foo",
            TNum "32",
            TPnc LNot,
            TPnc RParen,
            TId "bar",
            TPnc RCurl
          ]
        )
      ]
      $ \(input, expected) -> do
        it ("correctly lexes " ++ show input) $ do
          map snd (L.lex input) `shouldBe` expected
    pure ()
