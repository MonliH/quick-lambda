module RuntimeError where
import           Text.Printf                    ( printf )
import           Ast                            ( BinOp )
import           DisplayColor

data ErrorType = RuntimeTypeError | UnboundVariableError deriving (Show);

makeError :: ErrorType -> String -> String
makeError errTy msg =
  displayColor [Red] (printf "Error: %s: %s" (show errTy) msg)

binOpError :: (Show a, Show b) => BinOp -> a -> b -> String
binOpError op left right = makeError
  RuntimeTypeError
  (printf "cannot %s operands `%s` and `%s`" (show op) (show left) (show right))
