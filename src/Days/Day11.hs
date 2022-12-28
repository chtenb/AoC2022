module Days.Day11 where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text hiding (take, parseTest)
import Data.Void
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Int
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseMonkey `sepBy` "\n"

parseMonkey :: Parser Monkey
parseMonkey = do
  string "Monkey "
  nr <- decimal
  string ":\n"
  items <- parseItems
  operation <- parseOperation
  (test, divisor) <- parseTest
  return Monkey {nr, items, operation, test, divisor, inspections = 0}

parseItems :: Parser [WorryLevel]
parseItems = do
  string "  Starting items: "
  items <- decimal `sepBy` ", "
  string "\n"
  return items

parseOperation :: Parser Operation
parseOperation = do
  string "  Operation: new = "
  operand1 <- parseOperand
  operator <- parseOperator
  operand2 <- parseOperand
  string "\n"
  return $ \worryLevel -> operator (operand1 worryLevel) (operand2 worryLevel)
  where
    parseOperator :: Parser (WorryLevel -> WorryLevel -> WorryLevel)
    parseOperator = (string " + " $> (+)) <|> (string " * " $> (*))
    parseOperand :: Parser (WorryLevel -> WorryLevel)
    parseOperand = (string "old" $> id) <|> (const <$> decimal)

parseTest :: Parser (Test, Int64)
parseTest = do
  string "  Test: "
  divisor <- parseDivisor
  ifTrue <- parseIfTrue
  ifFalse <- parseIfFalse
  return (\worryLevel -> if worryLevel `mod` divisor == 0 then ifTrue else ifFalse, divisor)
  where
    parseDivisor = string "divisible by " *> decimal <* string "\n"
    parseIfTrue = string "    If true: " *> parseReceivingMonkey
    parseIfFalse = string "    If false: " *> parseReceivingMonkey
    parseReceivingMonkey = string "throw to monkey " *> decimal <* string "\n"

------------ TYPES ------------
type Input = [Monkey]

type OutputA = [Int]

type OutputB = Void

data Monkey = Monkey
  { nr :: MonkeyNr,
    items :: [WorryLevel],
    operation :: Operation,
    divisor :: Int64,
    test :: Test,
    inspections :: Int
  }

instance Show Monkey where
  show m = show m.nr ++ ": " ++ show m.items ++ " (inspected " ++ show m.inspections ++ " items)"

type MonkeyNr = Int

type WorryLevel = Int64

type Operation = WorryLevel -> WorryLevel

type Test = WorryLevel -> MonkeyNr

------------ PART A ------------
partA :: Input -> OutputA
partA input = map inspections $ simulateNRounds 10000 input

simulateNRounds n monkeys = foldl' (\ms _ -> simulateNMonkeys modulo l ms) monkeys [1 .. n]
  where
    l = length monkeys
    modulo = getModulo monkeys

-- l = 2

getModulo :: [Monkey] -> Int64
getModulo = product . map divisor

simulateNMonkeys :: Int64 -> Int -> Input -> Input
simulateNMonkeys modulo n monkeys = foldl' (throwAllItemsForMonkey modulo) monkeys [0 .. n - 1]

throwAllItemsForMonkey :: Int64 -> Input -> MonkeyNr -> Input
throwAllItemsForMonkey modulo monkeys nr =
  let monkey = monkeys !! nr
   in if null monkey.items then monkeys else throwAllItemsForMonkey modulo (inspectNextItemForMonkey modulo monkeys nr) nr

inspectNextItemForMonkey :: Int64 -> Input -> MonkeyNr -> Input
inspectNextItemForMonkey modulo monkeys nr =
  let monkey = monkeys !! nr
      (newMonkey, item, recipientNr) = throwNextItem modulo monkey
      recipient = monkeys !! recipientNr
      newRecipient = catchItem item recipient
   in replaceAt nr newMonkey $ replaceAt recipientNr newRecipient monkeys

catchItem :: WorryLevel -> Monkey -> Monkey
catchItem item recipient = recipient {items = recipient.items ++ [item]}

throwNextItem :: Int64 -> Monkey -> (Monkey, WorryLevel, MonkeyNr)
throwNextItem modulo monkey =
  let nextItem = head monkey.items
      remainingItems = tail monkey.items
      thrownItem = monkey.operation nextItem `mod` modulo
      recipientNr = monkey.test thrownItem
   in (monkey {items = remainingItems, inspections = monkey.inspections + 1}, thrownItem, recipientNr)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index item list = take index list ++ [item] ++ drop (index + 1) list

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
