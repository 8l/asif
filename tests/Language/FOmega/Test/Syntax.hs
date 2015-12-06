module Language.FOmega.Test.Syntax where

import Language.FOmega.Syntax
import Language.FOmega.Parse (parseExpr)
import Prelude hiding (pi)

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative

fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

fromLeft (Left msg) = msg
fromLeft (Right _) = error "fromLeft: got Right"

fromRight (Right x) = x
fromRight (Left msg) = error msg

isRight (Right _) = True
isRight (Left _) = False

tests = testGroup "Syntax" [
    renameTests
  , substTests
  , normTests
  ]


renameTests = 
  let go nm i o = testCase nm $ rename "x" "y" i @?= o
   
  in testGroup "rename" [
    go "lam type" (lam "x" (var "x") (var "x")) 
                  (lam "x" (var "y") (var "x"))
  , go "lam body" (lam "a" (var "b") (var "x"))   
                  (lam "a" (var "b") (var "y"))
  , go "lam body shadowed" 
                  (lam "x" (var "a") (var "x"))   
                  (lam "x" (var "a") (var "x"))
  , go "pi type"  (pi "x" (var "x") (var "x")) 
                  (pi "x" (var "y") (var "x"))
  , go "pi body"  (pi "a" (var "b") (var "x")) 
                  (pi "a" (var "b") (var "y"))
  , go "pi body shadowed"  
                  (pi "x" (var "a") (var "x")) 
                  (pi "x" (var "a") (var "x"))
  , go "decl type" (decl "x" (var "x") (var "a") (var "a"))
                   (decl "x" (var "y") (var "a") (var "a"))
  , go "decl def"  (decl "x" (var "a") (var "x") (var "a"))
                   (decl "x" (var "a") (var "y") (var "a"))
  , go "decl body" (decl "a" (var "a") (var "a") (var "x"))
                   (decl "a" (var "a") (var "a") (var "y"))
  , go "decl body shadowed" 
                   (decl "x" (var "a") (var "a") (var "x"))
                   (decl "x" (var "a") (var "a") (var "x"))
  ]

substTests = testGroup "subst" [
    avoidCapture
  ]
             
avoidCapture = testCase "avoid capture" $ do
  s <- show . (+1) <$> nextSymId
  r <- subst "x" (var "y") (lam "y" star $ var "x") 
  r @?= (lam s star $ var "y")
  
normTests = 
  let go nm src1 src2 = do
        let Right tm1 = parseExpr src1
            Right tm2 = parseExpr src2
        testCase nm $ norm tm1 @?= tm2
  in    
  testGroup "norm" [
    go "under lambdas"
       "\\a:*. \\x:a. (\\y:a. y) x"
       "\\a:*. \\x:a. x"

  , go "under Pis"
       "Pi a:*. (\\b:*. b) a"
       "Pi a:*. a"

  , go "on right side of applications"
       "\\a:*. \\x:a. \\f:a -> a. f ((\\y:a.y) x)"
       "\\a:*. \\x:a. \\f:a -> a. f x"
  ]

