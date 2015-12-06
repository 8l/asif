module Language.FOmega.Test.Derivation where

import Language.FOmega.Syntax (Tm, star, box, pi, var, lam, genSym)
import qualified Language.FOmega.Syntax as S
import Language.FOmega.Derivation hiding (
  star, box, pi, lam, var,
  typecheck, typecheckEnv)
import qualified Language.FOmega.Derivation as D
import Language.FOmega.Parse (parseExpr,loadPgmFile,builtin,builtinFrom,parseEnv)

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative
import Control.Monad.Trans.Except
import Prelude hiding (pi)
import System.IO.Unsafe (unsafePerformIO)

tests = testGroup "Derivation" [
    typecheckTests
  , parsedTests
  , fOmegaFileTests
  , preQuoteBaseTyTests
  , preQuoteTyTests
  , quoteTyEquivTests
  , tcQuoteTyTests
  , preQuoteTmTests
  , tcExpandQuotationsTests
  , unquoteTests
  , synthesizeTypeTests
  , stripTests
  ]

(-->) a b = pi "" a b

typecheckWithCfg cfg env tm = do
  r <- runExceptT $ D.typecheck cfg env tm
  case r of
    Left e -> fail $ unlines ["typecheck failed for:", show tm, showErr e]
    Right v -> return v

typecheck = typecheckWithCfg varQCfg 

typecheckEnvWithCfg' cfg env1 env2 = do
  r <- runExceptT $ D.typecheckEnv cfg env1 env2
  either (fail . showErr) return r

typecheckEnvWithCfg cfg env = typecheckEnvWithCfg' cfg [] env
typecheckEnv = typecheckEnvWithCfg varQCfg 

typecheckEnv' = typecheckEnvWithCfg' varQCfg 

typecheckTests = 
  let go nm tm ty = testCase nm $ do
                        tm' <- typecheck [] tm 
                        view tm' @?= tm
                        viewTy tm' @?= ty
      [x,_A] = map var $ words "x A"
  in
  testGroup "typecheck" [
    go "Star" star box
  , go "* -> *" (star --> star) box
  , go "\\A:*.\\x:A.x" (lam "A" star $ lam "x" _A $ x) (pi "A" star $ _A --> _A)
  ]

parsedTests =
  let ok tm ty = testCase tm $ do
                   let Right tm' = parseExpr tm
                       Right ty' = parseExpr ty
                   ty'' <- typecheck [] tm'
                   viewTy ty'' @?= ty'
  in
  testGroup "typecheck" [
    ok "* -> *" "#"
  , ok "Pi A:*. A" "*"
  ]

fOmegaFileTests =
  let go nm = testCase nm $ do
                src <- loadPgmFile
                         "fomega/lib" 
                         ("fomega/" ++ nm ++ ".fw")
                typecheck [] $ S.pgm2Tm src
                return ()
  in testGroup "FOmega Files" [
    go "id"
  , go "polyId"
  , go "cps"
  , go "cpsCBV"
  , go "isAbs"
  , go "pap"
  , go "size"
  , go "nf"
  ]

showView :: Tm a -> String
showView = show . view

showErr (AppError e1 e2 t1 t2) =
  unlines ["AppError", showView e1, showView e2, showView t1, showView t2]
showErr (VarError x) = "Undefined variable: " ++ x
showErr (PiError x l r) =
  unlines ["AppError", x, showView l, showView r]

unM :: M a -> a
unM act = unsafePerformIO $ do
  Right x <- runM (error "unM: no QCfg") [] act
  return x

varQCfg = 
  let u = S.Var (Der D.box) "U"
  in QCfg {
    qU      = u
  , qUOp    = S.var "UOp"
  
  , qRK     = "k"

  , qKF     = S.var "KF"

  , qAbs    = S.var "Abs"
  , qApp    = S.var "App"
  , qTAbs   = S.var "TAbs"
  , qTApp   = S.var "TApp"

  , q_abs    = "abs"
  , q_app    = "app"
  , q_tabs   = "tabs"
  , q_tapp   = "tapp"

  , qPExp   = S.var "PExp"
  , qExp    = S.Var (Der $ unM $ u D.==> D.star) "Exp"
  , qId     = S.var "Id"
  }

testEnvWithCfg :: QCfg -> IO D.Env
testEnvWithCfg cfg = do
  let vk = qRK cfg
  kF <- typecheckBuiltin "KF"
  return $ [(vk,kF)]

testEnv = testEnvWithCfg varQCfg

{-- TODO: move next two definitions to main Derivation module --}
newBuiltinQCfg :: IO QCfg
newBuiltinQCfg = do
  [k,abs,app,tabs,tapp] <- sequence $ take 5 $ repeat genSym
  builtinQCfg k abs app tabs tapp

builtinQCfg k abs app tabs tapp = do
  u <- builtin "U"
  tcU <- typecheck [] u

  _Exp <- builtin "Exp"
  tcExp <- typecheck [] _Exp

  QCfg <$> pure tcU
       <*> builtin "UOp"

       -- case function variable names
       <*> pure k 
       <*> builtin "KF"

       <*> pure abs
       <*> pure app
       <*> pure tabs
       <*> pure tapp

       -- Exp
       <*> builtin "Abs"
       <*> builtin "App"
       <*> builtin "TAbs"
       <*> builtin "TApp"
       <*> builtin "PExp"
       <*> pure tcExp
       <*> builtin "Id"


preQuoteBaseTyTests =
  let go env s e = 
        testCase s $ do
          let Right ty = parseExpr s
              Right e' = parseExpr e
          der <- typecheck env ty 
          qt <- runQ (preQuoteTy der) varQCfg
          view qt @?= e'
  in testGroup "quoteBaseTy" [
    go [("A", D.star)] "A" "A"
  , go [("A", D.star)] "A -> A" "k A -> k A"
  , go [] "Pi A:*. A" "Pi A:*. k A"
  ]

preQuoteTyTests =
  let go env s e = 
        testCase s $ do
          let Right ty = parseExpr s
              Right e' = parseExpr e
          der <- typecheck env ty 
          qt <- runQ (preQuoteTy der) varQCfg
          view qt @?= e'
  in testGroup "preQuoteTy" [
    go [("A", D.star)] "A" "A"
  , go [] "\\A:*. A" "\\A:*. A"
  , go [] "\\A:*. A -> A" "\\A:*. k A -> k A"
  , go [] "\\A:* -> *. Pi B:*. A B" 
          "\\A:* -> *. Pi B:*. k (A B))"
  , go [("A", unM $ D.star D.==> D.star), ("B", D.star)] "A B" "A B"
  ]

quoteTyEquivTests =
  let go env s1 s2 =
        testCase s1 $ do
          cfg <- builtinQCfg "k" "abs" "app" "tabs" "tapp"
          let Right t1 = parseExpr s1
              Right t2 = parseExpr s2
          der <- typecheck env t1
          n1 <- S.norm <$> runQ (preQuoteTy der) cfg
          let n2 = S.norm $ t2
          n1 @?= n2
  in testGroup "quoteTy Equiv" [
    go [("A", D.star)] "A" "A"
  , go [] "\\A:*. A" "\\A:*. A"
  , go [] "\\A:*. A -> A" "\\A:*. k A -> k A "
  , go [] "\\A:* -> *. Pi B:*. A B" 
          "\\A:* -> *. Pi B:*. k (A B))"
  , go [("A", unM $ D.star D.==> D.star), ("B", D.star)] "A B" "A B"
  ]

tcQuoteTyTests = 
  let go env ty k = 
        testCase ty $ do
          cfg <- builtinQCfg "k" "abs" "app" "tabs" "tapp"
          let Right ty' = parseExpr ty
              Right env' = parseEnv env
          env'' <- typecheckEnv env'
          k' <- builtin k
          der <- typecheck env'' ty'
          qTy <- S.norm <$> runQ (quoteTy der) cfg
          qDer <- typecheck env'' qTy
          let normK = S.norm k'
          S.erase (D.ty qDer) @?= normK

  in testGroup "tc quoteTy" [
    go [("A", "*")] "A" "U"
  , go [("A", "*")] "A -> A" "U"
  , go [] "Pi A:*. A -> A" "U"
  , go [] "Pi A:* -> *. Pi B:*. A (B -> B)" "U"
  ]

ctorEnv cfg = do
  let tc tm = typecheckWithCfg cfg [] (tm cfg) 
  let sk = qRK cfg
  let [sAbs, sApp, sTAbs, sTApp] = map (\f -> f cfg) [q_abs,q_app,q_tabs,q_tapp]
  kF <- tc qKF
  [_Abs, _App, _TAbs, _TApp] <- mapM tc [qAbs, qApp, qTAbs, qTApp]
  return [ (sk,kF)
         , (sAbs,    ty _Abs)
         , (sApp,    ty _App)
         , (sTAbs,   ty _TAbs)
         , (sTApp,   ty _TApp)
         ]

substCfg cfg tm = 
  S.subst "U" (S.erase $ qU cfg) =<<
  S.subst "UOp" (qUOp cfg) =<<
  S.subst "KF" (qKF cfg) =<<
  S.subst "PExp" (qPExp cfg) tm

parseAndTypecheckEnv envSrc = do
  let unRight (Right x) = x
      env = map (\(x,t) -> (x, unRight $ parseExpr t)) envSrc
  typecheckEnv env

preQuoteTmTest env1 e1 env2 e2 = testCase e1 $ do
  let Right e1' = parseExpr e1
      Right e2' = parseExpr e2
      Right env1' = parseEnv env1
      Right env2' = parseEnv env2
  env <- parseAndTypecheckEnv
         [ ("k",    "* -> *")
         , ("Abs",  "(* -> *) -> *")
         , ("App",  "(* -> *) -> *")
         , ("TAbs",  "(* -> *) -> *")
         , ("TApp",  "(* -> *) -> *")
         , ("abs",  "Pi A:*. Pi B:*. (k A -> k B) -> k (k A -> k B)")
         , ("app",  "Pi A:*. Pi B:*. k (k A -> k B) ->  k A -> k B")
         , ("tabs", "Pi A:*. (Pi T:*. (Pi B:*. k B -> T) -> A -> T) -> A -> k A")
         , ("tapp", "Pi A:*. k A -> (Pi B:*. (A -> k B) -> k B)")
         , ("Id",   "* -> *")
         ]

  env1'' <- typecheckEnv' env env1'
  der1 <- typecheck (env ++ env1'') e1'

  let [vk,vabs,vapp,vtabs,vtapp] = words "k abs app tabs tapp"
  builtinCfg <- builtinQCfg vk vabs vapp vtabs vtapp

  ctorEnv' <- ctorEnv builtinCfg

  let cfg = varQCfg { qU    = qU builtinCfg
                    , qUOp  = qUOp builtinCfg
                    , qPExp = qPExp builtinCfg 
                    , qKF   = qKF builtinCfg 
                    }

  let mySubstCfg x = substCfg cfg x

  env2'' <- typecheckEnvWithCfg' cfg ctorEnv' =<< 
            mapM (\(nm,ty) -> (,) <$> pure nm <*> mySubstCfg ty) env2'
  e2'' <- mySubstCfg e2' 

  der2 <- typecheckWithCfg cfg (env ++ env2'') e2''

  n1 <- S.norm <$> runQ (preQuoteTm der1) cfg 
  n2 <- S.norm <$> runQ (expandQuotations der2) cfg 

  n1 @?= n2

preQuoteTmTests =
  let go = preQuoteTmTest in
  testGroup "preQuoteTm" [
    go [("A", "*"), ("a", "A")] "a"
       [("A", "*"), ("a", "k A")]
       "a"
  , go [("A", "*")] "\\x:A. x" 
       [("A", "*")] $ unlines [
       "abs A A (",
       "\\x:k A. ",
       "x)"
       ]
  , go [("A", "*"), ("a", "A"), ("f", "A -> A")] "f a" 
       [("A", "*"), ("a", "k A"), ("f", "k (k A -> k A)")]
       "app A A f a"
  , go [] "\\A:*.\\x:A.x" 
       [] $ unlines [
        "tabs (Pi A:*. k (k A -> k A)) ",
        "(\\A:*. \\f:(Pi B:*. k B -> A). \\e:(Pi A:*. k (k A -> k A)).",
        " f (k (Pi A:*. A) -> k (Pi A:*. A)) (e (Pi A:*. A)))",
        "(\\A:*. abs A A (\\x:k A. x))"
       ]
  , go [("e", "Pi A:*. A -> A"), ("B", "*")] "e (B -> B)" 
       [("e", "k (Pi A:*. k (k A -> k A))"), ("B", "*")] $ unlines [
       "tapp ",
       " (Pi A:*. k (k A -> k A))",
       " e",
       " (k (k B -> k B) -> k (k B -> k B))",
       " (\\ e : (Pi A:*. k (k A -> k A)).",
       "  e (k B -> k B))"
       ]
  ]

typecheckBuiltin src = do
  tm <- builtin src
  typecheck [] tm

testRFuns = words "k abs app tabs tapp"
testCfg = do
  let [vk,vabs,vapp,vtabs,vtapp] = testRFuns
  builtinQCfg vk vabs vapp vtabs vtapp

tcExpandQuotationsTest envSrc tmSrc tySrc = testCase tmSrc $ do
  let [vk,vabs,vapp,vtabs,vtapp] = words "k abs app tabs tapp"
  cfg <- builtinQCfg vk vabs vapp vtabs vtapp
  let typecheck = typecheckWithCfg cfg
  testEnv' <- testEnv
  
  tm <- builtinFrom ["Repr","CPS", "IsAbs", "Size", "NF"] tmSrc
  ty <- builtinFrom ["Repr","CPS", "IsAbs", "Size", "NF"] tySrc
  let Right env1 = parseEnv envSrc

  env2  <- typecheckEnv env1
  tmDer <- typecheck env2 tm 
  tyDer <- typecheck env2 ty

  qTm <- runQ (expandQuotations tmDer) cfg
  qTy <- runQ (expandQuotations tyDer) cfg

  qTmDer <- typecheck (testEnv' ++ env2) qTm

  let got = S.norm $ S.erase (D.ty qTmDer)
      expected = S.norm $ S.erase qTy
  got @?= expected

tcExpandQuotationsTests = 
  let go = tcExpandQuotationsTest in
  testGroup "tc expandQuotations" [
    go [("A","*")] "[A]" "U"
  , go [("A","*")] "[A -> A]" "U"
  , go [] "[Pi A:* -> *. Pi B:*. A B]" "U"
  , go [("A","*")] "Exp [A]" "*"
  , go [("A","*")] 
       "[\\x:A.x]"  
       "Exp [A -> A]"
  , go [("A","*")] 
       "[\\f:A -> A. \\x:A.f x]"  
       "Exp [(A -> A) -> A -> A]"
  , go [] "[\\A:*. \\x:A. x]"
          "Exp [Pi A:*. A -> A]"
  , go [("B","*")] 
       "[(\\A:*. \\x:A. x) (B -> B)]"
       "Exp [(B -> B) -> B -> B]"
  , go [("B","*")] 
       "[\\x:(Pi A:*. A -> A). x (B -> B)]"
       "Exp [(Pi A:*. A -> A) -> (B -> B) -> B -> B]"
  , go []
       "[\\x:(Pi A:*. A -> A). \\B:*. x (B -> B)]"
       "Exp [(Pi A:*. A -> A) -> (Pi B:*. (B -> B) -> B -> B)]"
  , go [] "[unquote]" "Exp [Pi A:U. Exp A -> unU A]"
  , go [] "cps [Pi A:U. Exp A -> CPS A] [cps]"
          "CPS [Pi A:U. Exp A -> CPS A]"
  , go [] "isAbs [Pi A:U. Exp A -> Bool] [isAbs]"
          "Bool"
  , go [] "nf [Pi A:U. Exp A -> Bool] [nf]"
          "Bool"
  , go [] "size [Pi A:U. Exp A -> Nat] [size]"
          "Nat"
  ]

unquoteTest envSrc getSrc expectSrc = testCase getSrc $ do
  let [vk,vabs,vapp,vtabs,vtapp] = words "@k @abs @app @tabs @tapp"
  cfg <- builtinQCfg vk vabs vapp vtabs vtapp
  let typecheck = typecheckWithCfg cfg
  testEnv' <- testEnvWithCfg cfg
  
  get <- builtinFrom ["Repr", "Prelude", "CPS", "IsAbs", "Size"] getSrc
  expect <- builtinFrom ["Repr", "Prelude", "CPS", "IsAbs", "Size"] expectSrc
  let Right env1 = parseEnv envSrc

  env2      <- typecheckEnv env1
  getDer    <- typecheck env2 get
  expectDer <- typecheck env2 expect
  let expectTy = S.norm $ S.erase $ D.ty expectDer

  -- check types before quote expansion
  (S.norm $ S.erase $ D.ty getDer) @?= expectTy

  qGet <- runQ (expandQuotations getDer) cfg
  qGetDer <- typecheck (testEnv' ++ env2) qGet

  -- check types after quote expansion
  (S.norm $ S.erase $ D.ty qGetDer) @?= expectTy

  -- check terms
  (S.norm $ S.erase qGetDer) @?= (S.norm $ S.erase expectDer)


unquoteTests =
  let go = unquoteTest in
  testGroup "unquoteTests" [
    go [("A","*")] 
       "unquote [A -> A] [\\x:A. x]"
       "\\x:A.x"
  , go [("A","*")] 
       "unquote [A -> A] [(\\f:A -> A. f) (\\x:A. x)]"
       "\\x:A.x"
  , go []
       "unquote [Pi A:*. A -> A] [\\A:*. \\x:A. x]"
       "\\A:*. \\x:A. x"
  , go [("B","*")]
       "unquote [(B -> B) -> B -> B] [(\\A:*. \\x:A. x) (B -> B)]"
       "\\x:B -> B. x"
  , go [("B","*")]
       ("unquote [(B -> B) -> B -> B] " ++
        "      [(\\A:*. \\x:A. x) (Pi B:*. B -> B) (\\B:*. \\x:B. x) (B -> B)]")
       "\\x:B -> B. x"
  , go []
       "unquote [Pi A:U. Exp A -> unU A] [unquote]"
       "unquote"
  , go []
       "unquote [Pi A:U. Exp A -> CPS A] [cps]"
       "cps"
  , go []
       "unquote [Pi A:U. Exp A -> Bool] [isAbs]"
       "isAbs"
  , go [("B","*"), ("b","B")]
       "unquote [Pi A:*. A -> A] [\\A:*. \\x:A. x] B b"
       "b"
  , go []
       "unquote [Pi A:U. Exp A -> Nat] [size]"
       "size"
  ]
  

synthesizeTypeTest kSrc = testCase kSrc $ do
  let Right k = parseExpr kSrc
  kDer <- typecheck [] k
  t <- typecheck [] $ synthesizeType kDer
  ty t @?= kDer

synthesizeTypeTests = 
  let go = synthesizeTypeTest in
  testGroup "synthesizeType" [
    go "*"
  , go "* -> *"
  , go "(* -> * -> *) -> * -> (* -> *) -> *"
  ]

stripTest tySrc stripSrc = testCase tySrc $ do
  let Right t = parseExpr tySrc
      S.Pi _ x k b = t
      _F = S.var "F"
  kDer <- typecheck [] k
  
  let strip_t = strip _F kDer (S.lam x k b)
  let Right expected = parseExpr stripSrc

  strip_t @?= expected

stripTests = 
  let go = stripTest in
  testGroup "strip" [
    go "Pi A:*.A" $ unlines [
       "\\A:*. \\f : (Pi B:*. F B -> A). ",
       "\\e:(Pi A:*. F ((\\A:*.A) A)).",
       "f ((\\A:*.A) (Pi T:*.T)) (e (Pi T:*. T))"
     ]
  ]
  
