import Test.Framework (defaultMain)

import qualified Language.FOmega.Test.Syntax as Syntax
import qualified Language.FOmega.Test.Derivation as Der
import qualified Language.FOmega.Test.Parse as Parse

main = defaultMain tests

tests = [
    Syntax.tests,
    Der.tests,
    Parse.tests
  ]
