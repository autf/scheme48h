import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | String String
             | Number Integer
             | Bool Bool
             deriving Show

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ String str

parseAtom :: Parser LispVal
parseAtom = do
  c <- letter <|> symbol
  cs <- many (letter <|> symbol <|> digit)
  return $ case c:cs of
    "#t" -> Bool True
    "#f" -> Bool False
    atom -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseString
            <|> parseAtom
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   l <- try parseList <|> parseDottedList
                   char ')'
                   return l

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  e <- parseExpr
  return $ List [Atom "quote", e]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "<failed>: " ++ show err
  Right expr -> show expr

main :: IO ()
main = do
  (arg:_) <- getArgs
  putStrLn . readExpr $ arg
