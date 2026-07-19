module Main

import Async
import Db
import Exports

main : IO ()
main = do
  exportCommonJs "create" create
  exportCommonJs "get" getExternal
  exportCommonJs "put" put
  exportCommonJs "putOr" putOr
  exportCommonJs "putIf" putIf
  exportCommonJs "putOrIf" putOrIf
  exportCommonJs "del" del
  exportCommonJs "delOrIf" delOrIf
  exportCommonJs "batch" batchExternal
  exportCommonJs "close" close
