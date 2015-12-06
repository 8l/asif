import System.Environment (getArgs)
import Language.FOmega.Test.Derivation (newBuiltinQCfg) 
import Language.FOmega.Syntax
import Language.FOmega.Derivation (TyError(..), runM, tc, runQ, expandQuotations)
import Language.FOmega.Parse (loadPgmFile, parseExpr)

Right _fst = parseExpr "\\x:*. \\y:*. x"
Right _snd = parseExpr "\\x:*. \\y:*. y"

unTyError :: Either TyError a -> IO a
unTyError = either (fail . show) return

runEquivTest tm = do
  qCfg <- newBuiltinQCfg
  der <- unTyError =<< runM qCfg [] (tc tm)   {-- Type check         --}
  tm <- runQ (expandQuotations der) qCfg      {-- Expand quotations  --}
  let a  = norm (app tm _fst)                 {-- select first term  --}
      b  = norm (app tm _snd)                 {-- select second term --}
  return $
    if a == b
      then "Succeeded."
      else unlines [ "Failed."
                   , "Not alpha-equivalent: " 
                   , "   " ++ take 25 (show a) ++ "..."
                   , "   " ++ take 25 (show b) ++ "..."
                   ] 

main = do
  [nm] <- getArgs
  src <- loadPgmFile "fomega/lib" nm
  putStrLn =<< runEquivTest (pgm2Tm src)
