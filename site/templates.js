export {emptyTemplate,overflowExample,sumExample,setupText}

const setupText =
`{-# LANGUAGE TypeApplications #-}
import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn, until)
import IOTasks

main :: IO ()
main = taskCheckWith args program specification
`

const emptyTemplate =
`args :: Args
args = stdArgs

program :: MonadTeletype io => io ()
program = undefined

specification :: Specification
specification = undefined

-- basic ValueSets
ints, nats :: ValueSet Integer
ints = Every
nats = Eq 0 \`Union\` GreaterThan 0

str :: ValueSet String
str = Every`

const sumExample =
`args :: Args
args = stdArgs

program :: MonadTeletype io => io ()
program = do
  n <- readLn @_ @Integer
  let
    loop s m
      | s > n  = print @_ @Integer m
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0

specification :: Specification
specification =
  readInput n nats AssumeValid <>
  until (sum' (allValues x) .>. currentValue n)
    (readInput x ints AssumeValid) <>
  writeOutput [Value $ length' $ as @[Integer] $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

-- basic ValueSets
ints, nats :: ValueSet Integer
ints = Every
nats = Eq 0 \`Union\` GreaterThan 0`

const overflowExample =
`args :: Args
args = stdArgs {checkOverflows = False}
-- with chekOverflows = True the constraint solver tries to avoid input
-- sequences that overflow/underflow the range of Ints
-- (checkOverflows stdArgs == False by default)

program :: MonadTeletype io => io ()
program = do
  n <- readLn @_ @Integer
  if n < 0
    then program
    else
      let
      loop 0 x = print @_ @Integer x
      loop m x = do
        print m
        i <- readLn
        loop (m-1) (x*i)
      in loop n 1

specification :: Specification
specification =
  readInput n nats UntilValid <>
  until (length' (as @[Integer] $ allValues x) .==. currentValue n)
    (writeOptionalOutput [Value $ currentValue n .-. length' (as @[Integer] $ allValues x)] <> readInput x ints AssumeValid) <>
  writeOutput [Value $ product' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

-- basic ValueSets
ints, nats :: ValueSet Integer
ints = Every
nats = Eq 0 \`Union\` GreaterThan 0

str :: ValueSet String
str = Every`
