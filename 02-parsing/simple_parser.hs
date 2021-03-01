import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- readExp :: String -> String
-- readExp input = case parse (spaces >> symbol) "lisp" input of
--   Left err -> "<failed>: " ++ show err
--   Right val -> "accepted"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | String String
             | Number Integer
             | Bool Bool

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
parseExpr = parseString <|> parseAtom <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "<failed>: " ++ show err
  Right _ -> "accepted!"

main :: IO ()
main = do
  (arg:_) <- getArgs
  putStrLn . readExpr $ arg
