{-# LANGUAGE ScopedTypeVariables #-}

module DumbCC.Parser.ParenTreeSpec where

import Data.Traversable
import DumbCC.Lexer
import qualified DumbCC.Lexer as L
import DumbCC.Parser.ParenTree
import Test.Hspec

spec :: Spec
spec = do
  describe "reduceParen" $ do
    _ <- for
      [ ( [ TNum "3",
            TPnc L.Add,
            TNum "2",
            TPnc L.Eq,
            TNum "5"
          ],
          [ PTTok $ TNum "3",
            PTTok $ TPnc L.Add,
            PTTok $ TNum "2",
            PTTok $ TPnc L.Eq,
            PTTok $ TNum "5"
          ]
        ),
        ( [ TId "five",
            TPnc L.LParen,
            TNum "32",
            TPnc L.RParen
          ],
          [ PTTok $ TId "five",
            PTParen Paren [PTTok $ TNum "32"]
          ]
        )
      ]
      $ \(input, expected :: [PTree]) ->
        ( do
            it ("parses " ++ show input ++ " as " ++ show expected) $ do
              let result = case detectParens input of
                    Left err -> error err
                    Right x -> x
              result `shouldBe` expected
        )
    pure ()
