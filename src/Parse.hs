module Parse where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Data.Functor
import           Ast
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pLambdaSymbol :: Parser String
pLambdaSymbol = choice [symbol "\\", symbol "λ"]

rws :: [String]
rws =
  [ "if"
  , "then"
  , "else"
  , "while"
  , "do"
  , "skip"
  , "true"
  , "false"
  , "not"
  , "and"
  , "or"
  ]
identifier :: Parser Name
identifier = Name <$> (lexeme . try) (p >>= check)
 where
  p = (:) <$> letterChar <*> Text.Megaparsec.many alphaNumChar
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

pVarName :: Parser Expr
pVarName = Var <$> identifier

pInteger :: Parser Lit
pInteger = LInt <$> lexeme L.decimal

rWord :: String -> Parser ()
rWord w = string w *> notFollowedBy alphaNumChar *> sc

pBool :: Parser Lit
pBool = (rWord "true" $> LBool True) <|> (rWord "false" $> LBool False)

pLiteral :: Parser Expr
pLiteral = Lit <$> choice [pInteger, pBool]

pLambda :: Parser Expr
pLambda = do
  pLambdaSymbol
  name <- identifier
  symbol "."
  Lam name <$> pTerm

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = do
  choice [pVarName, pLiteral, pLambda, parens pExpr]

binary :: Parser () -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ name)

pExpr :: Parser Expr
pExpr = makeExprParser pTerm [[binary sc App]]
