import Data.Maybe (fromJust)
import Text.Read (readMaybe)

data Op = PUSH | POP | PLUS | MINUS | PEEK deriving (Eq, Show)

wordAsOp :: String -> (Op, Maybe String)
wordAsOp word
  | word == "+" = (PLUS, Nothing)
  | word == "-" = (MINUS, Nothing)
  | word == "p" = (PEEK, Nothing)
  | word == "." = (POP, Nothing)
  | otherwise = (PUSH, Just word)

parse :: [String] -> [(Op, Maybe String)] -> [(Op, Maybe String)]
parse xs acc = foldl (\acc x -> acc ++ [wordAsOp x]) acc xs

main :: IO ()
main = do
  contents <- readFile "nolang.nol"
  let wordList = words contents
  let result = parse wordList []
  -- print $ show wordList
  -- print $ show result
  simulate result $ push 42 (push 7 (push 99 []))

type Stack = [Integer]

-- Push an Integer onto the stack
push :: Integer -> Stack -> Stack
push x stack = x : stack

-- Pop the top Integer (returns Maybe Integer and the new stack)
pop :: Stack -> (Integer, Stack)
pop [] = error "nothin on stack"
pop (x : xs) = (x, xs)

-- Peek at the top Integer without removing it
peek :: Stack -> Integer
peek [] = error "nothin on stack"
peek (x : _) = x

-- Check if the stack is empty
isEmpty :: Stack -> Bool
isEmpty = null

maybeStringToInt :: Maybe String -> Integer
maybeStringToInt (Just str) = fromJust (readMaybe str)
maybeStringToInt Nothing = error "Nothing passed to maybeStringToInt"

simulate :: [(Op, Maybe String)] -> Stack -> IO ()
simulate [] _ = putStr ""
simulate (x : xs) stack
  | fst x == POP = do
      let popped = pop stack
      simulate xs $ snd popped
  | fst x == PUSH = do
      simulate xs $ push (maybeStringToInt (snd x)) stack
  | fst x == PLUS = do
      let first_popped = pop stack
      let second_popped = pop $ snd first_popped
      let a = fst first_popped
      let b = fst second_popped

      let popped_stack = snd second_popped
      let new_stack = push (b + a) popped_stack
      simulate xs new_stack
  | fst x == MINUS = do
      let first_popped = pop stack
      let second_popped = pop $ snd first_popped
      let a = fst first_popped
      let b = fst second_popped

      let popped_stack = snd second_popped
      let new_stack = push (b - a) popped_stack
      simulate xs new_stack
  | fst x == PEEK = do
      print $ peek stack
      simulate xs stack
  | otherwise = error "Invalid operation"
