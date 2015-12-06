module Language.FOmega.Derivation where

import Language.FOmega.Syntax hiding (
  star, box, lam, pi, app, var, (==>)
  )

import qualified Language.FOmega.Syntax as S
import Prelude hiding (pi)
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Applicative

import System.IO.Unsafe

-- type derivation
data Der = Der (Tm Der)
         | TypeOfBox
  deriving Show

unDer (Der tm) = tm
unDer TypeOfBox = error "unDer TypeOfBox"

ty :: Tm Der -> Tm Der
ty = unDer . tmVal

view :: Tm a -> Tm ()
view = erase

viewTy :: Tm Der -> Tm ()
viewTy = view . unDer . tmVal

star, box :: Tm Der
star  = Star (Der box)
box   = Box TypeOfBox

rules = [ ((star,star),star)
        , ((box,star),star)
        , ((box,box),box)
        ]

data TyError = PiError String (Tm Der) (Tm Der)
             | AppError (Tm Der) (Tm Der) (Tm ()) (Tm ())
             | VarError String
  deriving Show

type Env = [(String,Tm Der)] 

type M a = ReaderT Env (ReaderT QCfg (ExceptT TyError IO)) a
raise = lift . lift . throwE

askCfg = lift ask

asksCfg :: (QCfg -> a) -> M a
asksCfg f = lift (asks f)

askVarTy :: String -> M (Tm Der)
askVarTy x = do 
  mt <- asks (lookup x)
  case mt of
    Just ty -> return ty
    Nothing -> raise $ VarError x

(==>) a b = pi "" a b
infixr 5 ==>

runM :: QCfg -> Env -> M a -> IO (Either TyError a)
runM cfg env act =
  runExceptT (runReaderT (runReaderT act env) cfg)

pi :: String -> Tm Der -> Tm Der -> M (Tm Der)
pi x a b = do
  ty <- maybe (raise $ PiError x a b) return $ lookup (ty a, ty b) rules
  return $ Pi (Der ty) x a b

lam :: String -> Tm Der -> Tm Der -> M (Tm Der)
lam x a b  = do
  ty <- pi x a (ty b)
  return $ Lam (Der ty) x a b

app :: Tm Der -> Tm Der -> M (Tm Der)
app l r = do
  let tyR = ty r
  ty <- case norm (ty l) of
          (Pi _ x tyR' ty) ->
            let expected = norm (erase tyR')
                actual   = norm (erase tyR)
            in
            if expected == actual 
               then liftIO $ subst x r ty
               else raise $ AppError l r expected actual
          l' -> error $ unlines [
                    "unexpected app type: "
                  , "l = " ++ (take 100 $ show l)
                  , "ty l = " ++ (take 100 $ show l')
                  ]
  return $ App (Der ty) l r
  
var :: String -> M (Tm Der)
var x = do
  ty <- askVarTy x
  return $ Var (Der ty) x

typecheckEnv :: QCfg -> Env -> [(String,Tm ())] -> ExceptT TyError IO Env
typecheckEnv _ env [] = return env
typecheckEnv cfg env ((x,t):env') = do
  t' <- typecheck cfg env t
  typecheckEnv cfg (env ++ [(x,t')]) env'

typecheck :: QCfg -> Env -> Tm () -> ExceptT TyError IO (Tm Der)
typecheck cfg e tm = runReaderT (runReaderT (tc tm) e) cfg

tc :: Tm () -> M (Tm Der)
tc tm = 
  case tm of 
    Var _ x       -> var x
    Lam _ x t e   -> do t' <- tc t
                        e' <- local ((x,t'):) $ tc e
                        lam x t' e'
    Pi _ x l r    -> do l' <- tc l
                        r' <- local ((x,l'):) $ tc r
                        pi x l' r'
    App _ l r     -> do l' <- tc l
                        r' <- tc r
                        app l' r'
    Quote _ e     -> do e' <- tc e
                        case ty e' of
                          k | isKind k -> do cfg <- askCfg
                                             q <- liftIO $ runQ (quoteTy $ S.norm e') cfg
                                             tc q

                          t | isTy t -> do _Exp <- asksCfg qExp
                                           cfg <- askCfg 
                                           qt <- liftIO $ runQ (quoteTy $ S.norm t) cfg
                                           tcQt <- tc qt -- $ S.toCore qt
                                           _Exp_qt <- app _Exp tcQt
                                           return $ S.Quote (Der _Exp_qt) e'
                            | otherwise -> error "Quotation not a type or kind"
                           
    Decl _ x t d b -> do d' <- tc d
                         t' <- tc t
                         let expected = norm (erase t')
                         let got = norm (erase $ ty d')
                         if got /= expected
                           then fail $ unlines [
                                   "type error at declaration of " ++ x
                                  , "got: " ++ show got
                                  , "expected: " ++ show expected
                                  ]
                           else tc =<< liftIO (subst x d b)

    Star _       -> return star
    Box _        -> return box

isKind (Box _) = False
isKind x       = (== Box ()) . view . ty $ x

isTy (Box _) = False
isTy x       = isKind . ty $ x

isBaseTy (Box _) = False
isBaseTy x       = (== Star ()) . view . ty $ x

isTerm (Box _) = False
isTerm x       = isBaseTy . ty $ x
  
type Q a = ReaderT QCfg IO a

data QCfg = QCfg {
    qU    :: Tm Der
  , qUOp  :: Tm ()

  -- case function variable names
  , qRK   :: String

  -- types of case functions
  , qKF   :: Tm ()

  -- Exp
  , q_abs  :: String
  , q_app  :: String
  , q_tabs :: String
  , q_tapp :: String

  , qAbs  :: Tm ()
  , qApp  :: Tm ()
  , qTAbs :: Tm ()
  , qTApp :: Tm ()
  , qPExp :: Tm ()
  , qExp  :: Tm Der
  , qId   :: Tm ()
  }

askVar :: (QCfg -> String) -> Q (Tm ())
askVar f = S.var <$> asks f

_Abs = S.app <$> asks qAbs <*> askVar qRK
_App = S.app <$> asks qApp <*> askVar qRK
_TAbs = S.app <$> asks qTAbs <*> askVar qRK
_TApp = S.app <$> asks qTApp <*> askVar qRK

runQ = runReaderT

quoteArr :: Tm () -> Tm () -> Q (Tm ())
quoteArr t1 t2 = do k <- askVar qRK
                    return $ S.app k t1 S.==> S.app k t2

quoteAllBody :: String -> Tm Der -> Tm () -> Tm () -> Tm ()
quoteAllBody x k t _R = 
    S.pi x (erase k) $ 
    S.app _R t

quoteAll :: String -> Tm Der -> Tm () -> Q (Tm ())
quoteAll x k t = quoteAllBody x k t <$> askVar qRK

quoteTy :: Tm Der -> Q (Tm ())
quoteTy t = S.lam <$> asks qRK <*> asks qKF <*> preQuoteTy t
    
preQuoteTy :: Tm Der -> Q (Tm ())
preQuoteTy t = 
  let go = preQuoteTy in
  case t of
  Var _ x                    -> pure $ S.var x
  Pi _ _ t1 t2 | isBaseTy t1 -> join $ quoteArr <$> go t1 <*> go t2
  Pi _ x k t   | isKind k    -> join $ quoteAll x k <$> go t
  App _ t1 t2                -> S.app <$> go t1 <*> go t2
  Lam _ x k t                -> S.lam x (erase k) <$> go t


quoteVar :: Tm Der -> String -> Q (Tm ())
quoteVar _ x = return (S.var x)

quoteAbs :: String -> Tm Der -> Tm Der -> Q (Tm ())
quoteAbs x t b = do
  qT <- preQuoteTy t
  tx <- S.app <$> askVar qRK <*> pure qT
  S.app3 <$> askVar q_abs
         <*> pure qT
         <*> preQuoteTy (ty b)
         <*> (S.lam x tx <$> preQuoteTm b)

quoteApp :: Tm () -> Tm () -> Tm () -> Tm () -> Q (Tm ())
quoteApp t1 t2 e1 e2 = do
  mkApp <- askVar q_app
  return $ S.app4 mkApp t1 t2 e1 e2

strip :: Tm () -> Tm Der -> Tm () -> Tm ()
strip _K k b = unsafePerformIO $ do
    _A <- genSym
    _B <- genSym
    f <- genSym
    e <- genSym
    x <- genSym
    let t = synthesizeType k
    let bt = S.app b t

    return $ S.lam _A S.star $
             S.lam f (S.pi _B S.star $ S.app _K (S.var _B) S.==> (S.var _A)) $
             S.lam e (S.pi x (erase k) $ S.app _K (S.app b $ S.var x)) $
             S.app2 (S.var f) bt (S.app (S.var e) t)
  

quoteTAbs :: String -> Tm Der -> Tm Der -> Q (Tm ())
quoteTAbs x k b = do
  qB <- preQuoteTy (ty b)
  qT <- quoteAll x k qB
  _K <- askVar qRK
  S.app3 <$> askVar q_tabs
         <*> pure qT
         <*> pure (strip _K k (S.lam x (erase k) qB))
         <*> (S.lam x (erase k) <$> preQuoteTm b)

instFun :: Tm () -> Tm () -> Tm ()
instFun qPiBody qTyParam = 
  S.lam "e" qPiBody $ 
  S.app (S.var "e") qTyParam

quoteTApp :: Tm Der -> Tm Der -> Tm Der -> Q (Tm ())
quoteTApp instTy tm t = do
  let Pi _ x k piBody = norm (ty tm)
  qPiBody <- quoteAll x k =<< preQuoteTy piBody
  S.app4 <$> askVar q_tapp
         <*> pure qPiBody
         <*> preQuoteTm tm
         <*> preQuoteTy instTy
         <*> (instFun qPiBody <$> preQuoteTy t)
  

preQuoteTm :: Tm Der -> Q (Tm ())
preQuoteTm e | not (isTerm e) = error $ "Not a term: " ++ show e
preQuoteTm e = case e of
  Var _ x                    -> quoteVar (ty e) x
  Lam _ x t b | isBaseTy t   -> quoteAbs x t b
  Lam _ x k b | isKind k     -> quoteTAbs x k b
  App _ a b   | isTerm b     -> join $ quoteApp <$> preQuoteTy (ty b)
                                                <*> preQuoteTy (ty e) 
                                                <*> preQuoteTm a <*> preQuoteTm b
  App _ a t   | isTy t       -> quoteTApp (ty e) a t

quoteTm :: Tm Der -> Q (Tm ())
quoteTm e = 
  S.lam <$> asks qRK <*> asks qKF <*> (
  S.lam <$> asks q_abs <*> _Abs <*> (
  S.lam <$> asks q_app <*> _App <*> (
  S.lam <$> asks q_tabs <*> _TAbs <*> (
  S.lam <$> asks q_tapp <*> _TApp <*> (
  preQuoteTm e
  )))))

expandQuotations :: Tm Der -> Q (Tm ())
expandQuotations e | isTerm e = case e of
  Var _ x      -> return $ S.var x
  Lam _ x t b  -> S.lam x <$> expandQuotations t <*> expandQuotations b
  App _ a b    -> S.app <$> expandQuotations a <*> expandQuotations b
  Quote _ e    -> quoteTm e
  
expandQuotations t | isTy t = case t of
  Var _ x      -> return $ S.var x
  Lam _ x k b  -> S.lam x (erase k) <$> expandQuotations b
  App _ a b    -> S.app <$> expandQuotations a <*> expandQuotations b
  Pi _ x a b   -> S.pi x <$> expandQuotations a <*> expandQuotations b
  Quote _ t    -> quoteTy t

expandQuotations k | isKind k = return $ erase k

synthesizeType :: Tm Der -> Tm ()
synthesizeType k | isKind k =
  case k of
    Star _       -> S.pi "A" S.star (S.var "A") -- bottom
    Pi _ _ k1 k2 -> S.lam "" (erase k1) (synthesizeType k2)
    _            -> error "synthesizeType: input is not a kind"
