{-# LANGUAGE FlexibleInstances, IncoherentInstances, ViewPatterns #-}
module Language.FOmega.Syntax where

import Data.IORef
import System.IO.Unsafe
import Control.Applicative
import qualified Data.Set as S

import Control.Monad.Except

data Stmt = Stmts [Stmt]
          | SDecl String (Tm ()) (Tm ())
          | SLoad String
  deriving (Eq, Read, Show)

data Pgm = Pgm Stmt (Tm ())
  deriving (Eq, Read, Show)

data Tm a =
    Star a | Box a 
  | Lam a String (Tm a) (Tm a)
  | Pi a String (Tm a) (Tm a)
  | App a (Tm a) (Tm a)
  | Var a String
  | Decl a String (Tm a) (Tm a) (Tm a)
  | Quote a (Tm a)
  deriving (Read, Show)

(==>) a b = Pi () "" a b
infixr 5 ==>

{--
parse :: String -> Tm ()
parse = read --fromCore . C.pgm2Tm . read
--}

pgm2Tm (Pgm stmt tm) = compileStmt stmt tm
  
compileStmt :: Stmt -> Tm () -> Tm ()
compileStmt (Stmts stmts) = foldl (.) id $ map compileStmt stmts
compileStmt (SDecl nm ty bnd) = Decl () nm ty bnd


instance Eq (Tm a) where           
  e1 == e2 = unsafePerformIO $ aeq e1 e2
  
aeq :: Tm a -> Tm b -> IO Bool  
aeq (Var _ x) (Var _ y)     
  = return $ x == y
aeq (Lam _ n1 t1 b1) (Lam _ n2 t2 b2) 
  | n1 == n2 
    = (&&) <$> aeq t1 t2 <*> aeq b1 b2
  | otherwise 
    = do n <- genSym
         (&&) <$> aeq t1 t2 <*> aeq (rename n1 n b1) (rename n2 n b2)
aeq (Pi _ n1 t1 b1) (Pi _ n2 t2 b2) 
  | n1 == n2
    = (&&) <$> aeq t1 t2 <*> aeq b1 b2
  | otherwise    
    = do n <- genSym
         (&&) <$> aeq t1 t2 <*> aeq (rename n1 n b1) (rename n2 n b2)
aeq (Decl _ n1 t1 d1 b1) (Decl _ n2 t2 d2 b2) 
  | n1 == n2
    = and <$> sequence [aeq t1 t2, aeq d1 d2, aeq b1 b2]
  | otherwise 
    = do n <- genSym
         and <$> sequence [
             aeq t1 t2
           , aeq (rename n1 n d1) (rename n2 n d2)
           , aeq (rename n1 n b1) (rename n2 n b2)
           ]

aeq (App _ l1 r1) (App _ l2 r2) 
  = (&&) <$> aeq l1 l2 <*> aeq r1 r2

aeq (Quote _ a) (Quote _ b) = aeq a b
aeq (Star _)    (Star _)    = return True
aeq (Box _)     (Box _)     = return True
aeq _           _           = return False

runDiff a b = runExceptT $ diff a b

diff :: Tm () -> Tm () -> ExceptT (Tm (), Tm ()) IO ()
diff a@(Var _ x) b@(Var _ y)     
  | x == y    = return ()
  | otherwise = throwError (a,b)
diff (Lam _ n1 t1 b1) (Lam _ n2 t2 b2) 
  = do n <- liftIO genSym
       diff t1 t2
       diff (rename n1 n b1) (rename n2 n b2)
diff (Pi _ n1 t1 b1) (Pi _ n2 t2 b2) 
  = do n <- liftIO genSym
       diff t1 t2 
       diff (rename n1 n b1) (rename n2 n b2)
diff (Decl _ n1 t1 d1 b1) (Decl _ n2 t2 d2 b2) 
  = do n <- liftIO genSym
       diff t1 t2
       diff (rename n1 n d1) (rename n2 n d2)
       diff (rename n1 n b1) (rename n2 n b2)

diff (App _ l1 r1) (App _ l2 r2) 
  = do diff l1 l2
       diff r1 r2

diff (Quote _ a) (Quote _ b) = diff a b
diff (Star _)    (Star _)    = return ()
diff (Box _)     (Box _)     = return ()
diff a           b           = throwError (a,b)


lam   = Lam ()
pi    = Pi ()
var   = Var ()
app   = App ()
decl  = Decl ()
star  = Star ()
box   = Box ()
quote = Quote ()

app2 a b c           = app (app a b) c
app3 a b c d         = app (app2 a b c) d
app4 a b c d e       = app (app3 a b c d) e
app5 a b c d e f     = app (app4 a b c d e) f
app6 a b c d e f g   = app (app5 a b c d e f) g
app7 a b c d e f g h = app (app6 a b c d e f g) h

symRef :: IORef Int
symRef = unsafePerformIO $ newIORef 0

nextSymId :: IO Int
nextSymId = do i <- readIORef symRef
               writeIORef symRef (i+1)
               return i
               
genSym :: IO String
genSym = fmap show $ nextSymId               

genSyms :: Int -> IO [String]
genSyms n = sequence $ take n $ repeat genSym


tmVal :: Tm a -> a
tmVal t = 
  case t of
    Star a         -> a
    Box a          -> a
    Lam a _ _ _    -> a
    Pi a _ _ _     -> a
    App a _ _      -> a
    Var a _        -> a
    Decl a _ _ _ _ -> a
    Quote a _      -> a

-- optimized special case of subst: when substituting a variable for
-- another (fresh) variable, don't need to rename binders. No risk of
-- capture.
rename :: String -> String -> Tm a -> Tm a
rename nm1 nm2 e = 
  let go = rename nm1 nm2 in
  case e of
    Var a nm3 | nm1 == nm3 -> Var a nm2
              | otherwise  -> e
    App a t1 t2            -> App a (go t1) (go t2)
    Lam a nm3 t b 
      | nm1 == nm3         -> Lam a nm3 (go t) b
      | nm2 == nm3         -> error $ "rename Lam: not fresh! " ++ show (nm2,nm3)
      | otherwise          -> Lam a nm3 (go t) (go b)
    
    Pi a nm3 t b 
      | nm1 == nm3         -> Pi a nm3 (go t) b
      | nm2 == nm3         -> error $ "rename Pi: not fresh! " ++ show (nm2, nm3)
      | otherwise          -> Pi a nm3 (go t) (go b)
    Decl a nm3 t d b 
      | nm1 == nm3         -> Decl a nm3 (go t) (go d) b
      | nm2 == nm3         -> error $ "rename Decl: not fresh! " ++ show (nm1,nm3)
      | otherwise          -> Decl a nm3 (go t) (go d) (go b)
    Quote a t              -> Quote a (go t)
    Star a                 -> e
    Box a                  -> e

subst :: String -> Tm a -> Tm a -> IO (Tm a)
subst nm v e = 
  let go = subst nm v in
  case e of
    Var _ nm' | nm == nm' -> return v
              | otherwise -> return e
    App a t1 t2           -> App a <$> go t1 <*> go t2
    Lam a nm' t b 
      | nm == nm'         -> Lam a nm' <$> go t <*> pure b
      | null nm'          -> Lam a nm' <$> go t <*> go b  -- optimization
      | otherwise         -> do s <- genSym
                                Lam a s <$> go t <*> (go $ rename nm' s b)
    Pi a nm' t b 
      | nm == nm'         -> Pi a nm' <$> go t <*> pure b  
      | null nm'          -> Pi a nm' <$> go t <*> go b   -- optimization
      | otherwise         -> do s <- genSym
                                Pi a s <$> go t <*> (go $ rename nm' s b)
    Decl a nm' t d b 
      | nm == nm'         -> Decl a nm' <$> go t <*> go d <*> pure b
      | null nm'          -> Decl a nm' <$> go t <*> go d <*> go b
      | otherwise         -> do s <- genSym
                                Decl a s <$> go t <*> go d <*> (go $ rename nm' s b)
    Quote a t             -> Quote a <$> go t
    Star a                -> return e
    Box a                 -> return e

eval :: Tm a -> Tm a
eval e =
  case e of
    App a f x       -> case eval f of
                         Lam _ nm _ b -> eval $ unsafePerformIO $ subst nm x b
                         f'           -> App a f' x
    Lam _ _ _ _     -> e
    Pi _ _ _ _      -> e
    Var _ _         -> e
    Star _          -> e
    Box _           -> e
    Quote a e       -> Quote a $ eval e
    Decl _ nm _ d b -> eval (unsafePerformIO $ subst nm d b)
    
norm :: Tm a -> Tm a
norm e = 
  case eval e of
    Star a         -> Star a
    Box a          -> Box a
    Lam a nm t b   -> Lam a nm (norm t) (norm b)
    Pi a nm t b    -> Pi a nm (norm t) (norm b)
    App a f x      -> App a (norm f) (norm x)
    Var a x        -> Var a x
    Quote a e      -> Quote a $ norm e -- error "norm quote"
    Decl _ _ _ _ _ -> error "norm Decl"

fvs :: Tm a -> Tm (S.Set String)
fvs e =
  case e of
    Lam _ nm t b   -> let t' = fvs t
                          b' = fvs b
                          s  = tmVal t' `S.union` S.delete nm (tmVal b')
                      in Lam s nm t' b'    
    App _ f x      -> let f' = fvs f                         
                          x' = fvs x
                          s  = tmVal f' `S.union` tmVal x'
                      in App s f' x'
    Pi _ nm t b    -> let t' = fvs t
                          b' = fvs b
                          s  = tmVal t' `S.union` S.delete nm (tmVal b')
                      in Pi s nm t' b'                      
    Var _ nm       -> Var (S.singleton nm) nm                     
    Quote _ t      -> let t' = fvs t
                      in Quote (tmVal t') t'
    Decl _ nm t d b -> let t' = fvs t                     
                           d' = fvs d
                           b' = fvs b
                           s  = tmVal t' `S.union` tmVal d' `S.union`
                                S.delete nm (tmVal b')
                       in Decl s nm t' d' b'
                           
    Star _         -> Star S.empty
    Box _          -> Box S.empty

{-- 
etaContract :: Tm (S.Set String) -> Tm (S.Set String)
etaContract e = 
  let go = etaContract 
  in
  case e of
    Lam s1 nm t b ->
      let b' = go b
      in    
      case b' of 
        App _ l (Var _ nm') | nm == nm' && 
                              not (nm `S.member` tmVal l)  
                              -> l
        _                     -> Lam s1 nm t b'
    App s a b       -> App s (go a) (go b)
    Pi s nm t b     -> Pi s nm (go t) (go b)
    Quote s a       -> Quote s (go a)
    Decl s nm t d b -> Decl s nm (go t) (go d) (go b)
    Var _ _         -> e
    Star _          -> e
    Box _           -> e
  
eta = erase . etaContract . fvs
--}

erase :: Tm a -> Tm ()
erase e = 
  let go = erase in
  case e of
    Var _ x        -> Var () x
    App _ f a      -> App () (go f) (go a)
    Lam _ x t b    -> Lam () x (go t) (go b)
    Pi _ x t b     -> Pi () x (go t) (go b)
    Decl _ x t d b -> Decl () x (go t) (go d) (go b)
    Quote _ t      -> Quote () (go t)
    Star _         -> Star ()
    Box _          -> Box ()
    
isStar, isBox :: Tm a -> Bool    
isStar  (Star _)  = True
isStar  _         = False
isBox   (Box _)   = True
isBox   _         = False

