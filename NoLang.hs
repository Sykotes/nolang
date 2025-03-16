import Control.Exception
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Op = PUSH | POP | PLUS | MINUS | PEEK deriving (Eq, Show)

type Stack = [Integer]

sPush :: Integer -> Stack -> Stack
sPush x stack = x : stack

sPop :: Stack -> (Integer, Stack)
sPop [] = error "POP failed: Nothing on stack"
sPop (x : xs) = (x, xs)

sPeek :: Stack -> Integer
sPeek [] = error "PEEK failed: Nothing on stack"
sPeek (x : _) = x

sIsEmpty :: Stack -> Bool
sIsEmpty = null

maybeStringToInt :: Maybe String -> Integer
maybeStringToInt (Just str) = fromJust (readMaybe str)
maybeStringToInt Nothing = error "Nothing passed to maybeStringToInt"

wordAsOp :: String -> (Op, Maybe String)
wordAsOp word
  | word == "+" = (PLUS, Nothing)
  | word == "-" = (MINUS, Nothing)
  | word == "p" = (PEEK, Nothing)
  | word == "." = (POP, Nothing)
  | otherwise = (PUSH, Just word)

interpret :: [(Op, Maybe String)] -> IO ()
interpret xs = interpret' xs []
  where
    interpret' :: [(Op, Maybe String)] -> Stack -> IO ()
    interpret' [] _ = putStr ""
    interpret' (x : xs) stack
      | fst x == POP = do
          let popped = sPop stack
          interpret' xs $ snd popped
      | fst x == PUSH = do
          interpret' xs $ sPush (maybeStringToInt (snd x)) stack
      | fst x == PLUS = do
          let first_popped = sPop stack
          let second_popped = sPop $ snd first_popped
          let a = fst first_popped
          let b = fst second_popped

          let popped_stack = snd second_popped
          let new_stack = sPush (b + a) popped_stack
          interpret' xs new_stack
      | fst x == MINUS = do
          let first_popped = sPop stack
          let second_popped = sPop $ snd first_popped
          let a = fst first_popped
          let b = fst second_popped

          let popped_stack = snd second_popped
          let new_stack = sPush (b - a) popped_stack
          interpret' xs new_stack
      | fst x == PEEK = do
          print $ sPeek stack
          interpret' xs stack
      | otherwise = error "Invalid operation"

compile :: [(Op, Maybe String)] -> IO ()
compile = error "Compiling not implemented"

parse :: [String] -> [(Op, Maybe String)]
parse xs = parse' xs []
  where
    parse' :: [String] -> [(Op, Maybe String)] -> [(Op, Maybe String)]
    parse' xs acc = foldl (\acc x -> acc ++ [wordAsOp x]) acc xs

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: nolang COMMND <file>"
  putStrLn "    int <file>      interprets the file"
  putStrLn "    com <file>      compiles the file"

getValidArgs :: [String] -> (Bool, String)
getValidArgs args
  | null args = (False, "")
  | length args /= 2 = (False, "Wrong amount of arguments")
  | head args `notElem` ["int", "com"] = (False, "Invalid command")
  | ' ' `elem` head args = (False, "Invalid file name")
  | otherwise = (True, "")

run :: [String] -> IO ()
run args = do
  let file_path = args !! 1
  result <- try (readFile file_path) :: IO (Either IOError String)
  case result of
    Left err -> do
      putStrLn $ "Failed to read file - " ++ show err
    Right content -> do
      let wordList = words content
      let command = head args
      if command == "int"
        then interpret (parse wordList)
        else compile (parse wordList)

main :: IO ()
main = do
  args <- getArgs
  let validArgs = getValidArgs args
  if not (fst validArgs) 
    then do 
      putStr $ snd validArgs
      printUsage
    else do
      run args
