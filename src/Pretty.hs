module Pretty where

import           Text.PrettyPrint
import qualified Text.PrettyPrint              as PP
import           Ast

viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _         = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x         = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (App e1 e2) = go e1 [e2]
 where
  go (App a b) xs = go a (b : xs)
  go f         xs = (f, xs)
viewApp _ = error "not application"

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

class Pretty p where
    ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ (Name x) = text x

instance Pretty Expr where
  ppr p e = case e of
    Lit (LInt  a) -> text (show a)
    Lit (LBool b) -> text (show b)
    Var (Name x) -> text x
    App a b       -> parensIf (p > 0) $ ppr (p + 1) a <+> ppr p b
    Lam x a ->
      parensIf (p > 0)
        $     char '\955'
        PP.<> hsep (fmap (ppr 0) (viewVars e))
        <+>   text "->"
        <+>   ppr (p + 1) (viewBody e)

ppexpr :: Expr -> String
ppexpr = render . ppr 0
