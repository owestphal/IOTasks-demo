{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Context where

import Test.IOTasks as Constraints (Args)
import Test.IOTasks.Random as Random (Args)

type Context = String -> String

type family ContextArgs a where
  ContextArgs ConstraintType = Constraints.Args
  ContextArgs RandomType = Random.Args

constraintContext :: Context
constraintContext = context ConstraintContext

randomContext :: Context
randomContext = context RandomContext

context :: ContextType a -> Context
context ctxTy p =
  unlines
    ["{-# LANGUAGE TypeApplications #-}"
    ,"module Main where"
    ,"import Prelude hiding (putChar, putStr, putStrLn, print, getChar, getLine, readLn, until)"
    ,"import " ++ ioTasksImport ctxTy
    ,"import qualified System.IO as SIO"
    ,"import Control.Concurrent"
    ,"main :: IO ()"
    ,"main = do"
    ,"  SIO.hSetBuffering SIO.stdout SIO.NoBuffering >> taskCheckWith args program specification"
    ]
  <> p

data ConstraintType
data RandomType

data ContextType a where
  ConstraintContext :: ContextType ConstraintType
  RandomContext :: ContextType RandomType

ioTasksImport :: ContextType a -> String
ioTasksImport ConstraintContext = "Test.IOTasks"
ioTasksImport RandomContext = "Test.IOTasks.Random"
