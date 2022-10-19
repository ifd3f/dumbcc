{-# LANGUAGE ScopedTypeVariables #-}

module DumbCC.Parser.ExprSpec where

import DumbCC.Lexer
import qualified DumbCC.Lexer as L
import DumbCC.Parser.Types.Sugared
import qualified DumbCC.Parser.Types.Sugared as P
import Test.Hspec
import Data.Traversable

spec :: Spec
spec = do
  describe "parseExpr" $ do
    _ <- for
      [ ( [ TNum "3",
            TPnc L.Add,
            TNum "2",
            TPnc L.Eq,
            TNum "5"
          ],
          Just $ EBi P.Eq (EBi P.Add (ELit (LInt 3)) (ELit (LInt 5))) (ELit (LInt 5))
        ),
        ( [ TId "five",
            TPnc L.Mul,
            TNum "32"
          ],
          Just $ EBi P.Mul (EId "five") (ELit (LInt 32))
        )
      ]
      $ \(input, expected :: ExprS) ->
        ( do
            it ("parses " ++ show input ++ " as " ++ show expected) $ do
              parseExpr input `shouldBe` expected
        )
    pure ()
