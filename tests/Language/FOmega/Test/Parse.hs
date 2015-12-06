module Language.FOmega.Test.Parse where

--import Language.FOmega.Core as C
import Language.FOmega.Syntax as S
import qualified Language.FOmega.Parse as P

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Prelude hiding (pi)

(-->) a b = pi "" a b
infixr 4 -->

fromRight = either (error.show) id

tests = testGroup "parse" [
    parseExprTests
  , parseStmtTests
  ]

parseExprTests = 
  let ok src tm = testCase src $ 
                    let tm' = fromRight $ P.parseExpr src
                    in tm' @?= tm
  in
  let [x,_A,_B] = map var $ words "x A B"
  in
  testGroup "parseExpr" [
    ok "*" star
  , ok "(*)" star
  , ok "(* -> * -> *)" $ star --> star --> star
  , ok "x A B *" $ foldl1 app [x,_A,_B,star]
  , ok "* -> A B -> * #" $ star --> app _A _B --> app star box
  , ok "\\A:*.\\x:A.x A" $ lam "A" star $ lam "x" _A $ app x _A
  , ok "\\A:*.Pi x:A.x A" $ lam "A" star $ pi "x" _A $ app x _A
  , ok "Pi A:*.\\ x:A -> *.x A" $ pi "A" star $ lam "x" (_A --> star) $ app x _A
  , ok "Pi A:*. A -> Pi B:*. B" $
       pi "A" star $ _A --> (pi "B" star _B)
  , ok "[x]" $ quote (var "x")
  , ok "* [*]" $ app star (quote star)
  , ok "* [*] [*]" $ app (app star (quote star)) (quote star)
  , ok "[*] -> [*]" $ quote star --> quote star
  , ok "[* * -> *]" $ quote $ (app star star) --> star
  ]

parseStmtTests = 
  let ok src stmt = testCase src $ 
                    let stmt' = fromRight $ P.parseStmt src
                    in stmt' @?= stmt
  in
  testGroup "parseStmt" [
    ok "%load \"Hello\";" (SLoad "Hello")
  , ok "%decl X:# = *;"   (SDecl "X" box star)
  ]
