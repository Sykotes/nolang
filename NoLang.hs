import Data.Maybe (fromJust)
import Control.Exception
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

simulate :: [(Op, Maybe String)] -> Stack -> IO ()
simulate [] _ = putStr ""
simulate (x : xs) stack
  | fst x == POP = do
      let popped = sPop stack
      simulate xs $ snd popped
  | fst x == PUSH = do
      simulate xs $ sPush (maybeStringToInt (snd x)) stack
  | fst x == PLUS = do
      let first_popped = sPop stack
      let second_popped = sPop $ snd first_popped
      let a = fst first_popped
      let b = fst second_popped

      let popped_stack = snd second_popped
      let new_stack = sPush (b + a) popped_stack
      simulate xs new_stack
  | fst x == MINUS = do
      let first_popped = sPop stack
      let second_popped = sPop $ snd first_popped
      let a = fst first_popped
      let b = fst second_popped

      let popped_stack = snd second_popped
      let new_stack = sPush (b - a) popped_stack
      simulate xs new_stack
  | fst x == PEEK = do
      print $ sPeek stack
      simulate xs stack
  | otherwise = error "Invalid operation"

parse :: [String] -> [(Op, Maybe String)] -> [(Op, Maybe String)]
parse xs acc = foldl (\acc x -> acc ++ [wordAsOp x]) acc xs

printUsage :: IO ()
printUsage = putStrLn "Usage: nolang <file>"

main :: IO ()
main = do
  args <- getArgs
  let file_path = head args
  if length args /= 1
    then do 
      putStrLn "Wrong amount of arguments\n"
      printUsage
    else do
      result <- try (readFile file_path) :: IO (Either IOError String)
      case result of
        Left err -> do
          putStrLn $ "Failed to read file - " ++ show err
        Right content -> do
          contents <- readFile $ head args
          let wordList = words contents
          simulate (parse wordList []) []
