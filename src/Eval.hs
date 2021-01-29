module Eval where
import           Data.IORef
import           Ast
import           DisplayColor
import           Data.HashMap                   ( Map )
import qualified Data.HashMap                  as HM
import           GHC.IO.Exception               ( ioException )

data Value = VBool Bool
           | VInt Int
           | VClosure (Thunk -> IO Value)

instance Show Value where
  show (VBool    bool) = show bool
  show (VInt     int ) = show int
  show (VClosure _   ) = "<closure>"

type Thunk = () -> IO Value
type Env = Map Name (IORef Thunk)

envLookup :: Name -> Env -> IO (IORef Thunk)
envLookup (Name name) env = case HM.lookup (Name name) env of
  Just th -> return th
  Nothing -> ioException $ userError $ displayColor
    [Red]
    ("error: unbound variable: `" ++ name ++ "`")

update :: IORef Thunk -> IO Value -> IO ()
update ref val = do
  writeIORef ref (\() -> val)
  return ()

force :: IORef Thunk -> IO Value
force ref = do
  val <- fmap (\a -> a ()) (readIORef ref)
  update ref val
  val

newThunk :: Env -> Name -> Expr -> (Thunk -> IO Value)
newThunk env name expr th = do
  th' <- newIORef th
  eval (HM.insert name th' env) expr

eval :: Env -> Expr -> IO Value
eval env expr = do
  case expr of
    -- Lookup thunk
    Var name      -> do
      val <- envLookup name env
      force val

    -- Save the lambda for later
    -- Only eval it durning function application
    Lam name expr -> return $ VClosure (newThunk env name expr)

    -- Run saved lambda on function application
    App fn   expr -> do
      VClosure closure <- eval env fn
      closure (\() -> eval env expr)

    Lit (LInt  int ) -> return $ VInt int
    Lit (LBool bool) -> return $ VBool bool
