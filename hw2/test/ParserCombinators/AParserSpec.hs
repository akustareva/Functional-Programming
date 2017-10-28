module ParserCombinators.AParserSpec
       ( main
       , spec
       ) where

import           Control.Applicative       ((<|>))
import           Data.Char                 (isDigit)
import           ParserCombinators.AParser
import           Test.Hspec

main :: IO ()
main = hspec spec

type Name = String

data Employee = Emp { name :: Name, phone :: String }
  deriving (Show, Eq)

parseName :: Parser Name
parseName = Parser $ \s -> parsePrefix s
  where
    parsePrefix :: String -> Maybe(Name, String)
    parsePrefix s = Just $ break isDigit s

parsePhone :: Parser String
parsePhone = Parser $ \s -> Just (s, "")

parseFullPhone :: Parser String
parseFullPhone = Parser $ \s -> Just $ span isDigitOrPlus s
  where
    isDigitOrPlus :: Char -> Bool
    isDigitOrPlus c = c == '+' || isDigit c

spec :: Spec
spec = do
    it "Functor instance" $ do
        let f s = if head s == '+' then s else "+" ++ s
        let short = runParser (fmap f parsePhone) "7(900)000-00-00"
        let full = runParser (fmap f parsePhone) "+7(900)000-00-00"
        short `shouldBe` Just("+7(900)000-00-00", "")
        full `shouldBe` Just("+7(900)000-00-00", "")
    it "Applicative instance" $ do
        let parseEmp = Emp <$> parseName <*> parsePhone
        let emp      = runParser parseEmp "anya8888"
        emp `shouldBe` Just (Emp {name="anya", phone= "8888"}, "")
    it "abParser" $ do
        runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
        runParser abParser "aebcdf" `shouldBe` Nothing
    it "abParser_" $ do
        runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
        runParser abParser_ "aecdef" `shouldBe` Nothing
    it "intPair" $ do
        runParser intPair "12 34" `shouldBe` Just ([12, 34], "")
        runParser intPair "122ra" `shouldBe` Nothing
    it "Alternative instance" $ do
        let f = parseFullPhone <|> parsePhone
        runParser f "888" `shouldBe` Just ("888", "")
        runParser f "+888" `shouldBe` Just ("+888", "")
    it "intOrUppercase" $ do
        runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
        runParser intOrUppercase "XYZ"     `shouldBe` Just ((), "YZ")
        runParser intOrUppercase "foo"     `shouldBe` Nothing

