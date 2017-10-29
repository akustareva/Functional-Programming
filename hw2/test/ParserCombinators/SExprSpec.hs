module ParserCombinators.SExprSpec
       ( main
       , spec
       ) where

import           Data.Char                 (isUpper)
import           ParserCombinators.AParser
import           ParserCombinators.SExpr
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "zeroOrMore" $ do
        runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
        runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("", "abcdeFGh")
    it "oneOrMore" $ do
        runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
        runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing
    it "spaces" $ do
        runParser spaces "  utfd" `shouldBe` Just ("  ", "utfd")
        runParser spaces "utfd"   `shouldBe` Just ("", "utfd")
    it "ident" $ do
        runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
        runParser ident "foo33fA"    `shouldBe` Just ("foo33fA", "")
        runParser ident "2bad"       `shouldBe` Nothing
        runParser ident ""           `shouldBe` Nothing
    it "skipSpaces" $ do
        runParser (skipSpaces posInt) "   123  "   `shouldBe` Just (123, "")
        runParser (skipSpaces posInt) "   123  1 " `shouldBe` Just (123, "1 ")
    it "parseAtom" $ do
        runParser parseAtom "   an123  "        `shouldBe` Just (I "an123","")
        runParser parseAtom "   12345  "        `shouldBe` Just (N 12345,"")
        runParser parseAtom " an123  rest 5 "   `shouldBe` Just (I "an123","rest 5 ")
        runParser parseAtom "  12345  rest  "   `shouldBe` Just (N 12345,"rest  ")
    it "parseSExpr" $ do
        runParser parseSExpr "5"                   `shouldBe` Just (A (N 5), "")
        runParser parseSExpr "  5   "              `shouldBe` Just (A (N 5), "")
        runParser parseSExpr "foo3  "              `shouldBe` Just (A (I "foo3"),"")
        runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` Just (Comb [A (I "bar"), 
                                                                          Comb [A (I "foo")], 
                                                                          A (N 3), 
                                                                          A (N 5), 
                                                                          A (N 874)], "")
