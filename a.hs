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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "<failed>: " ++ show err
  Right val -> val

showVal :: LispVal -> String
showVal (String cs) = "\"" ++ cs ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordList xs ++ ")"
showVal (DottedList xs x) = "(" ++ unwordList xs ++ "." ++ showVal x ++ ")"

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval v@(String _) = v
eval v@(Number _) = v
--eval v@(Atom _) = v
eval v@(Bool _) = v
eval (List [Atom "quote", xs]) = xs
eval (List (Atom fn : xs)) = apply fn $ map eval xs

apply :: String -> [LispVal] -> LispVal
apply fn xs = maybe (Bool False) ($ xs) $
  lookup fn primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op vs = Number $ foldl1 op $ map unpackNum vs

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
  if null parsed
  then 0
  else fst $ parsed !! 0
--unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = print . eval . readExpr . head =<< getArgs
