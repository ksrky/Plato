module Plato.NicifierSpec where

-- import Plato.Parsing.OpParser

import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Driver.Context
import Plato.Driver.Monad
import Plato.Parsing
import Plato.Parsing.OpParser
import Plato.Syntax.Parsing

spec :: Spec
spec = do
        describe "Fixity resolution" $ do
                it "x + y" $ do
                        test "x + y" `shouldReturn` "(x + y)"
                it "x + y * z" $ do
                        test "x + y * z" `shouldReturn` "(x + (y * z))"
                it "x ++ y ++ z" $ do
                        test "x ++ y ++ z" `shouldReturn` "(x ++ (y ++ z))"
                it "x > y > z" $ do
                        test "x > y > z" `shouldThrow` anyException
                it "f $ g x" $ do
                        test "f $ g $ h x y" `shouldReturn` "(f $ (g $ h x y))"
        describe "File test" $ do
                it "05.pla" $ do
                        test_file "05.pla"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool"
                                               , "not where {b -> case b of {True -> False; False -> True}}"
                                               ]
                it "07.pla" $ do
                        test_file "07.pla"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool"
                                               , "not where {True -> False}"
                                               , "not where {False -> True}"
                                               ]
                it "14.pla" $ do
                        test_file "14.pla"
                                `shouldReturn` [ "data List a where {Nil : List a; :: : a -> List a -> List a}"
                                               , "data T where {T1 : T; T2 : T; T3 : T}"
                                               , "f : List T"
                                               , "f where {-> (T1 :: (T2 :: (T3 :: Nil)))}"
                                               ]
                it "16.pla" $ do
                        test_file "16.pla"
                                `shouldReturn` [ "data :,: a b where {:,: : a -> b -> a :,: b}"
                                               , "fst : {a b} a :,: b -> a"
                                               , "fst where {(x :,: _) -> x}"
                                               , "snd : {a b} a :,: b -> b"
                                               , "snd where {(_ :,: y) -> y}"
                                               , "curry : {a b c} (a :,: b -> c) -> a -> b -> c"
                                               , "curry where {f x y -> f ((x :,: y))}"
                                               , "uncurry : {a b c} (a -> b -> c) -> a :,: b -> c"
                                               , "uncurry where {f (x :,: y) -> f x y}"
                                               , "swap : {a b} a :,: b -> b :,: a"
                                               , "swap where {(x :,: y) -> (y :,: x)}"
                                               ]

fixityEnv :: FixityEnv
fixityEnv =
        M.fromList
                [ (varName "*", Fixity 7 Leftfix)
                , (varName "+", Fixity 6 Leftfix)
                , (varName "-", Fixity 6 Leftfix)
                , (varName "++", Fixity 5 Rightfix)
                , (varName ">", Fixity 4 Nonfix)
                , (varName "$", Fixity 0 Rightfix)
                ]

test :: T.Text -> IO String
test inp = do
        exp <- runReaderT (parseExpr inp) =<< initSession
        return $ show $ pretty exp

test_file :: String -> IO [String]
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        return $ map (show . pretty) pssyn
                )
                =<< initSession