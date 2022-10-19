{-# LANGUAGE ScopedTypeVariables #-}

module DumbCC.Parser.ParenTreeSpec where

import Data.Traversable
import DumbCC.Lexer
import qualified DumbCC.Lexer as L
import DumbCC.Parser.ParenTree
import DumbCC.Parser.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "readToken" $ do
    it "places open braces" $ do
      let result = runParser [] $ readToken (TPnc LBrac)
      result `shouldBe` (Right (), [StOpen Brac])
    it "closes braces when close-brace encountered" $ do
      let stackNum = StPT . PTTok . TNum
      let result =
            runParser
              [ stackNum "3",
                stackNum "2",
                stackNum "1",
                StOpen Curl
              ]
              $ readToken
                (TPnc RCurl)
      result
        `shouldBe` ( Right (),
                     [ StPT $
                         PTParen
                           Curl
                           [ PTTok $ TNum "1",
                             PTTok $ TNum "2",
                             PTTok $ TNum "3"
                           ]
                     ]
                   )

  describe "detectParens" $ do
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
        ),
        ( map snd $ L.lex "12 {(foo 32 !) bar}",
          [ PTTok $ TNum "12",
            PTParen
              Curl
              [ PTParen
                  Paren
                  [ PTTok $ TId "foo",
                    PTTok $ TNum "32",
                    PTTok $ TPnc L.LNot
                  ],
                PTTok $ TId "bar"
              ]
          ]
        )
      ]
      $ \(input, expected :: [PTree]) ->
        ( do
            it ("correctly matches " ++ show input) $ do
              let result = case detectParens input of
                    Left err -> error err
                    Right x -> x
              result `shouldBe` expected
        )
    pure ()
