module Eval where
import           Data.IORef
import           Ast
import           RuntimeError
import           Data.HashMap                   ( Map )
import qualified Data.HashMap                  as HM
import           GHC.IO.Exception               ( ioException )
import           Control.Monad.Trans.Except

data Value = VBool Bool
           | VInt Int
           | VLambda (Thunk -> IO EEV)

instance Eq Value where
  (==) (VBool l) (VBool r) = l == r
  (==) (VInt  l) (VInt  r) = l == r
  (==) _         _         = False
  (/=) l r = not (l == r)

instance Show Value where
  show (VBool   bool) = if bool then "true" else "false" ++ " (bool)"
  show (VInt    int ) = show int ++ " (int)"
  show (VLambda _   ) = "<lambda>"

type Thunk = () -> IO EEV
type Error = String
type EEV = Either Error Value
type Env = Map Name (IORef Thunk)

envLookup :: Name -> Env -> IO (Maybe (IORef Thunk))
envLookup (Name name) env = case HM.lookup (Name name) env of
  Just th -> return $ Just th
  Nothing -> return Nothing

update :: IORef Thunk -> IO EEV -> IO ()
update ref val = do
  writeIORef ref (\() -> val)
  return ()

force :: IORef Thunk -> IO EEV
force ref = do
  val <- fmap (\a -> a ()) (readIORef ref)
  update ref val
  val

newThunk :: Env -> Name -> Expr -> (Thunk -> IO EEV)
newThunk env name expr th = do
  th' <- newIORef th
  eval (HM.insert name th' env) expr

getBoolFunc :: Env -> Bool -> IO EEV
getBoolFunc env bool = eval
  env
  (Lam (Name "a") (Lam (Name "b") (Var (Name (if bool then "a" else "b")))))

eval :: Env -> Expr -> IO EEV
eval env expr = do
  case expr of
    -- Lookup thunk
    Var (Name name) -> do
      mth <- envLookup (Name name) env
      case mth of
        Just th -> force th
        Nothing -> return
          ((Left $ makeError UnboundVariableError
                             ("variable `" ++ name ++ "` is not defined")
           ) :: EEV
          )

    -- Save the lambda for later
    -- Only eval it durning function application
    Lam name expr -> return $ Right $ VLambda (newThunk env name expr)

    -- Run saved lambda on function application
    App fn   expr -> do
      closure <- eval env fn
      case closure of
        Right (VLambda closure) -> closure (\() -> eval env expr)
        Right (VBool   bool   ) -> do
          Right (VLambda fn) <- getBoolFunc env bool
          fn (\() -> eval env expr)
        Right other -> return $ Left $ makeError
          RuntimeTypeError
          ('`' : show other ++ "` is not a lambda")
        Left err -> return $ Left err

    Lit (LInt  int ) -> return $ Right $ VInt int
    Lit (LBool bool) -> return $ Right $ VBool bool

    BinOp Eq left right ->
      evalBinOp env Sub left right (\a b -> Right $ VBool $ a == b)

    BinOp op left right -> evalBinOpNumber env op left right $ case op of
      Add -> (\a b -> Right $ VInt $ a + b)
      Mul -> (\a b -> Right $ VInt $ a * b)
      Sub -> (\a b -> Right $ VInt $ a - b)

evalBinOpNumber
  :: Env -> BinOp -> Expr -> Expr -> (Int -> Int -> EEV) -> IO EEV
evalBinOpNumber env op left right fn = evalBinOp
  env
  op
  left
  right
  (\l r -> case (l, r) of
    (VInt l, VInt r) -> fn l r
    _                -> Left $ binOpError op l r
  )

evalBinOp :: Env -> BinOp -> Expr -> Expr -> (Value -> Value -> EEV) -> IO EEV
evalBinOp env op left right fn = do
  eLeft  <- eval env left
  eRight <- eval env right
  case (eLeft, eRight) of
    (Right l  , Right r ) -> return $ fn l r
    (Left  err, _       ) -> return eLeft
    (Right _  , Left err) -> return eRight
