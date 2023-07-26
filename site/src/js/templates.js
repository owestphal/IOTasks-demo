export
  { emptyTemplate
  , productExample
  , sumExample
  , sumExampleWithOutput
  , sumToZero
  , singlePath
  , fullTree
  , stringExample
  , constraintSetup
  , randomSetup
  }

const constraintSetup =
`{-# LANGUAGE TypeApplications #-}
import Prelude hiding (putChar, putStr, putStrLn, print, getChar, getLine, readLn, until)
import Test.IOTasks

main :: IO ()
main = taskCheckWith args program specification
`

const randomSetup =
`{-# LANGUAGE TypeApplications #-}
import Prelude hiding (putChar, putStr, putStrLn, print, getChar, getLine, readLn, until)
import Test.IOTasks.Random

main :: IO ()
main = taskCheckWith args program specification
-- HINT: The Args type for random testing differs from the constraint-based one!`

const emptyTemplate =
`-- Example loaded: Basic setup

args :: Args
args = stdArgs

program :: MonadTeletype io => io ()
program = undefined

specification :: Specification
specification = undefined
`

const sumExample =
`-- Example loaded: Summation

args :: Args
args = stdArgs

program :: MonadTeletype io => io ()
program = do
  n <- readLn
  let
    loop s m
      | m == n  = print s
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0

specification :: Specification
specification =
  readInput n nats AssumeValid <>
  tillExit (
    branch (length' (as @[Integer] $ allValues x) .==. currentValue n)
      exit
      (readInput x ints AssumeValid)
  ) <>
  writeOutput [Value $ sum' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"
`

const sumExampleWithOutput =
`-- Example loaded: Summation with optional output

args :: Args
args = stdArgs

program :: MonadTeletype io => io ()
program = do
  n <- readLn
  let
    loop s m
      | m == n  = print s
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0

specification :: Specification
specification =
  readInput n nats AssumeValid <>
  tillExit (
    branch (length' (as @[Integer] $ allValues x) .==. currentValue n)
      exit
      (writeOptionalOutput [Value $ currentValue n .-. length' (as @[Integer] $ allValues x)] <>
      readInput x ints AssumeValid)
  ) <>
  writeOutput [Value $ sum' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"
`

const sumToZero =
`-- Example loaded: Sum to zero

args :: Args
args = stdArgs

program :: MonadTeletype io => io ()
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


specification :: Specification
specification =
  readInput x ints AssumeValid <>
  tillExit (
    readInput x ints AssumeValid <>
    branch (currentValue' x 1 .+. currentValue x .==. intLit 0)
      exit
      nop
    ) <>
  writeOutput [Value $ length' $ as @[Integer] $ allValues x]
  where x = intVar "x"
`

const singlePath =
`-- Example loaded: Single path

args :: Args
args = stdArgs

program :: MonadTeletype io => io ()
program = do
  getLine
  getLine
  pure ()

specification :: Specification
specification =
  tillExit (
    branch (length' (as @[Integer] $ allValues x) .<. intLit 2)
      (readInput x nats AssumeValid)
      exit
  )
  where x = intVar "x"
`

const productExample =
`-- Example loaded: Product (overflow checks)

args :: Args
args = stdArgs {checkOverflows = True}
-- with checkOverflows = True the constraint solver tries to avoid input
-- sequences that overflow/underflow the range of Ints
-- (checkOverflows stdArgs == False by default)

program :: MonadTeletype io => io ()
program = do
  n <- readLn
  let
    -- change first parameter to Int to trigger rejection of the
    -- program with checkOverflows = False
    loop :: MonadTeletype io => Integer -> Integer -> io ()
    loop p m
      | m == n  = print p
      | otherwise = do
        x <- readLn
        loop (p*x) (m+1)
  loop 1 0

specification :: Specification
specification =
  readInput n nats AssumeValid <>
  tillExit (
    branch (length' (as @[Integer] $ allValues x) .==. currentValue n)
      exit
      (readInput x ints AssumeValid)
  ) <>
  writeOutput [Value $ product' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"
`

const fullTree =
`-- Example loaded: Exponentially many sat. paths

args :: Args
args = stdArgs{ maxIterationUnfold = 10 }

program :: MonadTeletype io => io ()
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

specification :: Specification
specification =
  readInput x ints AssumeValid <>
  tillExit (
    branch (sum' (as @[Integer] $ allValues x) .>. intLit 0)
      exit
      (readInput x ints AssumeValid <>
        branch (currentValue x .>. intLit 0)
          (writeOutput [Text "positive"])
          (writeOutput [Text "not positive"])
      )
  )
  where
    x = intVar "x"
`

const stringExample =
`-- Example loaded: String output

args :: Args
args = stdArgs

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

specification :: Specification
specification =
  tillExit (
    optionalTextOutput <>
    readInput x ints AssumeValid <>
    branch (currentValue x .==. intLit 0)
    exit
    (optionalTextOutput <>
     readInput y ints AssumeValid <>
     writeOutput [Wildcard <> Value (currentValue x .+. currentValue y) <> Wildcard])
  ) <>
  writeOutput [Wildcard <> Value (length' $ as @[Integer] $ allValues y) <> Wildcard]
  where
    x = intVar "x"
    y = intVar "y"
`
