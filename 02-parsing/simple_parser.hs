import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- spaces :: Parser String
-- spaces = many1 space

readExp :: String -> String
readExp input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "<failed>: " ++ show err
  Right val -> "accepted"

main :: IO ()
main = do
  (arg:_) <- getArgs
  putStrLn . readExp $ arg
