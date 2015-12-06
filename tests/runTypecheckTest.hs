import System.Environment (getArgs)

import Language.FOmega.Syntax
import Language.FOmega.Derivation 
import Language.FOmega.Test.Derivation (newBuiltinQCfg)
import Language.FOmega.Parse (loadPgmFile)

main = do
  [nm] <- getArgs
  src  <- loadPgmFile "fomega/lib" nm
  qCfg <- newBuiltinQCfg
  r    <- runM qCfg [] (tc $ pgm2Tm src)
  let msg = either (show) (const "Succeeded.") r
  putStrLn msg    
