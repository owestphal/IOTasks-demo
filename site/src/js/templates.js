export
  { emptyTemplate
  , productExample
  , sumExample
  , sumExampleWithOutput
  , sumToZero
  , singlePath
  , fullTree
  , stringExample
  , untilValidExample
  , greeter
  , palindrome
  , constraintSetup
  , randomSetup
  }

const constraintSetup =
`{-# LANGUAGE TypeApplications #-}
import Prelude hiding (putChar, putStr, putStrLn, print, getChar, getLine, readLn)
import Test.IOTasks

main :: IO ()
main = taskCheckWith args program specification
`

const randomSetup =
`{-# LANGUAGE TypeApplications #-}
import Prelude hiding (putChar, putStr, putStrLn, print, getChar, getLine, readLn)
import Test.IOTasks.Random

main :: IO ()
main = taskCheckWith args program specification
-- HINT: The Args type for random testing differs from the constraint-based one!`

const emptyTemplate =
`-- Example loaded: Basic setup

args :: Args
args = stdArgs

specification :: Specification
specification = undefined

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = undefined
`

const sumExample =
`-- Example loaded: Sum

args :: Args
args = stdArgs{ feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = HorizontalTrace } }

specification :: Specification
specification =
  readInput n nats AssumeValid <>
  whileNot (length' (as @[Integer] $ allValues x) .==. currentValue n) (
    readInput x ints AssumeValid
  ) <>
  writeOutput [resultOf $ sum' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

{- Write a program that calculates the sum of a series of integers
 - entered by the user. Your program should first prompt the user to
 - enter a single integer, n. This number will represent the total
 - number of integers the user will input next. After getting the
 - value of n, your program should read n integers, one at a time,
 - from the user. Once all n numbers have been entered, the program
 - should display the total sum of those numbers.
 -}

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  n <- readLn
  let
    loop s m
      | m == n  = print s
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0
`

const sumExampleWithOutput =
`-- Example loaded: Sum, with optional output

args :: Args
args = stdArgs{ feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = HorizontalTrace } }

specification :: Specification
specification =
  readInput n nats AssumeValid <>
  whileNot (length' (as @[Integer] $ allValues x) .==. currentValue n) (
    writeOptionalOutput [resultOf $ currentValue n .-. length' (as @[Integer] $ allValues x)] <>
    readInput x ints AssumeValid
  ) <>
  writeOutput [resultOf $ sum' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

{- Write a program that calculates the sum of a series of integers
 - entered by the user. Your program should first prompt the user to
 - enter a single integer, n. This number will represent the total
 - number of integers the user will input next. After getting the
 - value of n, your program should read n integers, one at a time,
 - from the user. While doing so, the program may print out, before
 - each input, how many values are still missing. Once all n numbers
 - have been entered, the program should display the total sum of
 - those numbers.
 -}

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  n <- readLn
  let
    loop s m
      | m == n  = print s
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0
`

const sumToZero =
`-- Example loaded: Sum to zero

args :: Args
args = stdArgs{ feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = HorizontalTrace } }

specification :: Specification
specification =
  readInput x ints AssumeValid <>
  (readInput x ints AssumeValid
  \`repeatUntil\` (valueBefore 1 x .+. currentValue x .==. intLit 0)) <>
  writeOutput [resultOf $ length' $ as @[Integer] $ allValues x]
  where
    x = intVar "x"

{- Write a program to help in a number sequence analysis. The aim of
 - this program is to read a series of integer numbers from the input
 - and determine after how many numbers read, the sum of the current
 - number and the previous number becomes zero.
 -}

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  x <- readLn
  loop x 1
  where
    loop x n = do
      y <- readLn
      let n' = n+1
      if x + y == 0
        then print $ n'
        else loop y n'
`

const singlePath =
`-- Example loaded: Single path

args :: Args
args = stdArgs{ feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = HorizontalTrace } }

specification :: Specification
specification =
  while (length' (as @[Integer] $ allValues x) .<. intLit 2) (
    readInput x nats AssumeValid
  )
  where x = intVar "x"

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  getLine
  getLine
  pure ()
`

const productExample =
`-- Example loaded: Product (potential for overflows)

args :: Args
args = stdArgs { avoidOverflows = True, feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = HorizontalTrace } }
-- set avoidOverflows = False to potentially generate input
-- sequences that overflow/underflow the range of Ints

specification :: Specification
specification =
  readInput n nats AssumeValid <>
  whileNot (length' (as @[Integer] $ allValues x) .==. currentValue n) (
    readInput x ints AssumeValid
  ) <>
  writeOutput [resultOf $ product' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

{- Write a program that performs the following actions:
 -
 - 1. Read an integer n from the standard input.
 - 2. Read n more integers one by one from the standard input.
 - 3. Calculate the product of these n integers.
 - 4. Print the resulting product.
 -}

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  n <- readLn
  let
    -- change first parameter to Int to trigger rejection of the
    -- program during testing with avoidOverflows = False
    loop :: MonadTeletype io => Integer -> Integer -> io ()
    loop p m
      | m == n  = print p
      | otherwise = do
        x <- readLn
        loop (p*x) (m+1)
  loop 1 0
`

const fullTree =
`-- Example loaded: Exponentially many sat. paths

args :: Args
args = stdArgs{ maxIterationUnfold = 10, feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = HorizontalTrace } }

specification :: Specification
specification =
  readInput x ints AssumeValid <>
  whileNot (sum' (as @[Integer] $ allValues x) .>. intLit 0) (
    readInput x ints AssumeValid <>
    branch (currentValue x .>. intLit 0)
      (writeOutput [text "positive"])
      (writeOutput [text "not positive"])
  )
  where
    x = intVar "x"

{- Write a program that works as follows:
 -
 - 1. The program first asks the user to input a number.
 -
 - 2. This initial number serves as a running total or sum.
 -
 - 3. The program then enters a loop of operations based on the running total:

    - If the running total is positive, the program stops and waits
      for no further input.

    - If the running total is not positive, the program prompts the
      user for another number.

    - After receiving the new number, the program gives feedback to
      the user, stating whether the newly provided number is positive
      or not.

    - The newly input number is added to the running total.

    - The loop continues until the running total becomes positive.
 -}

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  x <- readLn
  let
    loop s
      | s > 0 = pure ()
      | otherwise = do
        x <- readLn
        putStrLn $ if x > 0 then "positive" else "not positive"
        loop (s+x)
  loop x
`

const stringExample =
`-- Example loaded: Handling string output

args :: Args
args = stdArgs

specification :: Specification
specification =
  tillExit (
    anyOptionalOutput <>
    readInput x ints AssumeValid <>
    branch (currentValue x .==. intLit 0)
      exit
      (anyOptionalOutput <>
       readInput y ints AssumeValid <>
       writeOutput [wildcard <> resultOf (currentValue x .+. currentValue y) <> wildcard])
  ) <>
  writeOutput [wildcard <> resultOf (length' $ as @[Integer] $ allValues y) <> wildcard]
  where
    x = intVar "x"
    y = intVar "y"

{- Write a program that reads in two integers (negative integers too)
 - and prints out their sum. This behavior is repeated until the first
 - of the two numbers read is 0. The program then terminates (not
 - reading a second number), printing the count of additions
 - performed.
 -
 - You can add additional information to both the output of the
 - addition results as well as the final output. Furthermore you might
 - want to add additional outputs to indicate what the user has to do
 - next.
 -}

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = loop 0
  where
    loop n = do
      putStr "First number or 0 to exit: "
      x <- readLn
      if x == 0
        then do
          putStrLn "Exiting program"
          putStr "The number of additions performed was: "
          print n
        else do
          putStr "Second number: "
          y <- readLn
          putStr ("The sum of " ++ show x ++ " and " ++ show y ++ " is ")
          print (x + y)
          loop (n + 1)
`

const untilValidExample =
`-- Example loaded: Handling ill-formed inputs

args :: Args
args = stdArgs{ maxNegative = 2, maxIterationUnfold = 10, feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = VerticalTrace } }

specification :: Specification
specification =
  tillExit (
    anyOptionalOutput <>
    readInput x nats UntilValid <>
    branch (currentValue x .==. intLit 0)
      exit
      (anyOptionalOutput <>
       readInput y nats UntilValid <>
       writeOutput [wildcard <> resultOf (currentValue x .+. currentValue y) <> wildcard])
  ) <>
  writeOutput [wildcard <> resultOf (length' $ as @[Integer] $ allValues y) <> wildcard]
  where
    x = intVar "x"
    y = intVar "y"

{- Write a program that reads in two non-negative integers
 - and prints out their sum. This behavior is repeated until the first
 - of the two numbers read is 0. The program then terminates (not
 - reading a second number), printing the count of additions
 - performed.
 -
 - When reading a negative integer the program should prompt for a new
 - number, until the read number is non-negative.
 -
 - You can add additional information to both the output of the
 - addition results as well as the final output. Furthermore you might
 - want to add additional outputs to indicate what the user has to do
 - next.
 -}

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = loop 0
  where
    loop n = do
      putStr "First number or 0 to exit: "
      x <- readPositive
      if x == 0
        then do
          putStrLn "Exiting program"
          putStr "The number of additions performed was: "
          print n
        else do
          putStr "Second number: "
          y <- readPositive
          putStr ("The sum of " ++ show x ++ " and " ++ show y ++ " is ")
          print (x + y)
          loop (n + 1)

readPositive :: MonadTeletype io => io Integer
readPositive = do
  x <- readLn
  if x < 0
    then do
      putStrLn "I don't like negative numbers at all."
      putStr "Please try again: "
      readPositive
    else
      return x
`

const greeter =
`-- Example loaded: Greeter

args :: Args
args = stdArgs{ feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = VerticalTrace } }

specification :: Specification
specification =
  writeOutput [text "What is your name?"] <>
  readInput name str AssumeValid <>
  writeOutput [text "Hello, " <> resultOf (as @String $ currentValue name) <> text "!"]
  where
    name = stringVar "name"

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"
`
const palindrome =
`-- Example loaded: Palindrome

args :: Args
args = stdArgs{ feedbackStyle = FeedbackStyle { simplifyFeedback = True, traceStyle = VerticalTrace } }

specification :: Specification
specification =
  readInput x str AssumeValid <>
  writeOutput [text "is palindrome? "] <>
  branch (isPalindrome (currentValue x))
    (writeOutput [text "True"])
    (writeOutput [text "False"])
  where
    x = stringVar "x"

    isPalindrome :: Term k String -> Term k Bool
    isPalindrome x = x .==. reverse' x

program :: MonadTeletype io => io ()
-- program = head (interpret specification) {- use this line to execute the specification itself -}
program = do
  x <- getLine
  putStr "is palindrome? "
  print (reverse x == x)
`
