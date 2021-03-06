import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Except

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval v@(String _) = return v
eval v@(Number _) = return v
--eval v@(Atom _) = v
eval v@(Bool _) = return v
eval (List [Atom "quote", xs]) = return xs
eval (List (Atom fn : xs)) = apply fn =<< mapM eval xs
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn xs = maybe (throwError $ NotFunction "Unrecognized primitive function args" fn)
              ($ xs)
              (lookup fn primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ x@[_] = throwError $ NumArgs 2 x
numericBinop op vs = return . Number . foldl1 op =<< mapM unpackNum vs

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
  if null parsed
  then throwError $ TypeMismatch "number" $ String s
  else return . fst $ parsed !! 0
--unpackNum (List [n]) = unpackNum n
unpackNum nan = throwError $ TypeMismatch "number" nan

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVariable String String
               | Default String

showError :: LispError -> String
showError (UnboundVariable msg varname) = msg ++ ": " ++ varname
showError (NotFunction msg fn) = msg ++ ": " ++ fn
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++
                                     unwordList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser err) = "Parser error at " ++ show err

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right v) = v

main :: IO ()
main = do
  args <- getArgs
  putStrLn . extractValue . trapError . fmap show $ eval =<< (readExpr $ head args)

--   let val = (readExpr . head $ args) >>= eval >>= return . show
--   putStrLn $ extractValue $ trapError $ val

--   --(readExpr . head $ args) >>= show <*> eval >>= putStrLn . extractValue . trapError
-- --main = print =<< eval =<< readExpr . head =<< getArgs
