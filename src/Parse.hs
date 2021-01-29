module Parse where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Data.Functor
import           Data.Maybe                     ( fromMaybe )
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

identifier :: Parser Name
identifier = Name <$> fmap (: []) letterChar

pVarName :: Parser Expr
pVarName = Var <$> identifier

pInteger :: Parser Lit
pInteger = LInt <$> lexeme L.decimal

rWord :: String -> Parser ()
rWord w = string w *> notFollowedBy letterChar *> sc

pBool :: Parser Lit
pBool = (rWord "true" $> LBool True) <|> (rWord "false" $> LBool False)

pLiteral :: Parser Expr
pLiteral = Lit <$> choice [pInteger, pBool]

pArgs :: Parser (Expr -> Expr)
pArgs = fromMaybe id <$> optional
  (do
    ids <- many identifier
    return $ \expr -> foldr Lam expr ids
  )

pLambda :: Parser Expr
pLambda = do
  pLambdaSymbol
  name      <- identifier
  otherArgs <- pArgs
  symbol "."
  Lam name . otherArgs <$> pTerm

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = do
  choice [pLambda, pVarName, pLiteral, parens pExpr]

binary :: Parser () -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ name)

pExpr :: Parser Expr
pExpr = makeExprParser
  pTerm
  [ [binary sc App]
  , [binary (void (symbol "*")) (BinOp Mul)]
  , [ binary (void (symbol "+")) (BinOp Add)
    , binary (void (symbol "-")) (BinOp Sub)
    ]
  ]
