import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExp :: String -> String
readExp input = case parse symbol "lisp" input of
  Left err -> "<failed>: " ++ show err
  Right val -> "accepted"

main :: IO ()
main = do
  (arg:_) <- getArgs
  putStrLn . readExp $ arg
