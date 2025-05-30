{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Server (runMain) where

-- compilation imports
import GHC
import Data.IORef (newIORef,modifyIORef, readIORef)
import GHC.Paths (libdir)
import GHC.Driver.Session (GhcNameVersion(..), defaultFlushOut, defaultFatalMessager)
import GHC.Utils.Error
import GHC.Utils.Logger (LogFlags(..))
import GHC.Utils.Outputable (SDocContext(sdocStyle), renderWithContext, setStyleColoured, ($+$))
import Unsafe.Coerce (unsafeCoerce)

-- other imports
import Data.Text.Lazy (pack, unpack, replace)
import Control.Monad.IO.Class (liftIO)
import System.IO.Temp (withTempFile)
import System.IO (hPutStr,hClose, hSetBuffering, stdout, BufferMode(..))
import Control.Exception (catch, throw, Exception(..), SomeException(..))
import Control.Concurrent.Async (race_, concurrently_,)
import Control.Monad (void, forM_, replicateM_, when)
import Control.Concurrent.STM
import System.Timeout (timeout)

import Test.IOTasks as Constraints (Specification, Args(..), runSpecification)
import Test.IOTasks.Random as Random (Args(..), genInput)
import Test.IOTasks.Constraints (constraintTree, pathDepth, simplePaths, SimplePath, completePath)
import Test.IOTasks.Trace (normalizedTrace, showTraceN)
import Test.IOTasks.ValueSet (Size(..))
import Test.IOTasks.Z3 (evalPathScript, SatResult (..), satPathsQ, findPathInput, mkPathInjector, PathInjector (injectNegatives))

import Test.QuickCheck (generate)

import Context
import MonitoredIO
import Test.IOTasks.Overflow
import Test.QuickCheck.Gen (oneof)

runMain :: IO ()
runMain = do
  request <- getLine
  case request of
    "get_info" -> getServerInfo
    "send_src" -> do
      context <- getLine
      case context of
        "constraints" -> readSourceAndWait ConstraintContext
        "random" -> readSourceAndWait RandomContext
        _ -> putStrLn "unknown context"
    _ -> putStrLn "invalid request"

readSourceAndWait :: ContextType a -> IO ()
readSourceAndWait ctx = do
  hSetBuffering stdout NoBuffering
  src <- fmap ($ "") loop
  compileAndWait ctx src
  where
    loop :: IO ShowS
    loop = do
      l <- getLine
      case l of
        "EOF" -> pure id
        s -> (((s ++ "\n") ++) <>) <$> loop

getServerInfo :: IO ()
getServerInfo = do
  infoType <- getLine
  case infoType of
    "ghc_version" -> putStrLn =<< ghcVersion
    _ -> putStrLn "unknown info"

compileAndWait :: ContextType a -> ProgramSrc -> IO ()
compileAndWait ctx src = do
  putStrLn "compiling ..."
  res <- tryCompile ctx src
  case res of
    Left msg -> putStrLn "INFO: failure" >> putStrLn msg
    Right (m,s,p,a,temp) -> do
      putStrLn "INFO: success"
      void $ timeout (minutes 10) (sessionLoop $ newSession ctx m s temp a p)

minutes :: Int -> Int
minutes = (* (60 * 10^6))

data SessionState a where
  ConstraintSession :: Specification -> FilePath -> Constraints.Args -> IO () -> MonitoredIO () -> SessionState ConstraintType
  RandomSession :: Specification -> FilePath -> Random.Args -> IO () -> MonitoredIO () -> SessionState RandomType

newSession :: ContextType a -> IO () -> Specification -> FilePath -> ContextArgs a -> MonitoredIO () -> SessionState a
newSession RandomContext = newRandomSession
newSession ConstraintContext = newConstraintSession

newConstraintSession :: IO () -> Specification -> FilePath -> Constraints.Args -> MonitoredIO () -> SessionState ConstraintType
newConstraintSession m s t a = ConstraintSession s t a m

newRandomSession :: IO () -> Specification -> FilePath -> Random.Args -> MonitoredIO () -> SessionState RandomType
newRandomSession m s t a = RandomSession s t a m

specification :: SessionState a -> Specification
specification (ConstraintSession s _ _ _ _) = s
specification (RandomSession s _ _ _ _) = s

mainProgram :: SessionState a -> IO ()
mainProgram (RandomSession _ _ _ p _) = p
mainProgram (ConstraintSession _ _ _ p _) = p

ioProgram :: SessionState a -> MonitoredIO ()
ioProgram (RandomSession _ _ _ _ p) = p
ioProgram (ConstraintSession _ _ _ _ p) = p

tempPath :: SessionState a -> FilePath
tempPath (RandomSession _ t _ _ _) = t
tempPath (ConstraintSession _ t _ _ _) = t

sessionLoop :: forall a. SessionState a -> IO ()
sessionLoop st = case st of
  RandomSession{} -> randomSession
  ConstraintSession{} -> constraintsSession
  where
    randomSession :: (a ~ RandomType) => IO ()
    randomSession = do
      cmd <- getLine
      case cmd of
        "run" -> runProgram st >> randomSession
        "sample_input" -> sampleInput st >> randomSession
        "run_io" -> runIO st >> randomSession
        "run_spec" -> runSpec st >> randomSession
        "exit" -> pure ()
        "" -> randomSession
        _ -> error "unknown command"
    constraintsSession :: (a ~ ConstraintType) => IO ()
    constraintsSession = do
      cmd <- getLine
      case cmd of
        "run" -> runProgram st >> constraintsSession
        "smt_code" -> smtCode st >> constraintsSession
        "sample_input" -> sampleInput st >> constraintsSession
        "run_io" -> runIO st >> constraintsSession
        "run_spec" -> runSpec st >> constraintsSession
        "exit" -> pure ()
        "" -> constraintsSession
        _ -> error "unknown command"

runProgram :: SessionState a -> IO ()
runProgram st = do
  abortable (tempPath st) (mainProgram st)
  putStrLn "INFO: terminated"

-- only works if p does not read from stdin
-- otherwise use MonitoredIO
abortable :: FilePath -> IO () -> IO ()
abortable t p = race_ waitForAbort $ do
  void p `catch` (\(SomeException e) -> putStrLn $ clean t $ displayException e)

waitForAbort :: IO ()
waitForAbort = do
  msg <- getLine
  case msg of
    "~" -> pure ()
    _ -> waitForAbort

sampleInput :: SessionState a -> IO ()
sampleInput (RandomSession s t Random.Args{..} _ _)  = do
  n <- readLn
  abortable t $ replicateM_ n $ do
    i <- generate $ Random.genInput s maxInputLength (Size inputRange (fromIntegral $ inputRange `div` 5)) maxNegative
    print i
  putStrLn "INFO: terminated"
sampleInput (ConstraintSession s t Constraints.Args{..} _ _)  = do
  n <- readLn
  m <- readLn
  abortable t $ do
    nVar <- newTVarIO (Just m)
    qVar <- newTQueueIO
    let
      outputInputs :: Int -> IO ()
      outputInputs 0 = do
        atomically $ writeTVar nVar (Just 0)
      outputInputs x = do
        mp <- atomically $ readTQueue qVar
        case mp of
          Just p -> do
            injector <- mkPathInjector p solverTimeout solverMaxSeqLength avoidOverflows

            cp1 <- generate $ injectNegatives injector 0
            res1 <- findPathInput solverTimeout cp1 inputRange solverMaxSeqLength avoidOverflows
            success1 <- printSat res1

            let x' = x - if success1 then 1 else 0
            success2 <- if x' > 0
              then do
                cp2 <- generate $ injectNegatives injector maxNegative
                res2 <- findPathInput solverTimeout cp2 inputRange solverMaxSeqLength avoidOverflows
                printSat res2
              else pure False
            let x'' = x' - if success2 then 1 else 0
            outputInputs x''

          Nothing -> pure ()
    concurrently_
      (satPathsQ nVar solverTimeout (constraintTree s) maxIterationUnfold solverMaxSeqLength avoidOverflows qVar)
      (outputInputs n)
  putStrLn "INFO: terminated"

printSat :: Show a => SatResult a -> IO Bool
printSat (SAT x) = print x >> pure True
printSat _ = pure False

smtCode :: SessionState ConstraintType -> IO ()
smtCode (ConstraintSession s t Constraints.Args{..} _ _) = do
  n <- readLn
  abortable t $ do
    let (negativesNotNeeded,negativesNeeded) = relevantPaths n $ simplePaths n $ constraintTree s

    forM_ negativesNotNeeded $ \p -> do
      let cp = completePath [] p
      res <- evalPathScript solverTimeout cp inputRange solverMaxSeqLength avoidOverflows
      outputSMTProblem res
      putStrLn "INFO: end of smt problem"

    forM_ negativesNeeded $ \p -> do
      injector <- mkPathInjector p solverTimeout solverMaxSeqLength avoidOverflows
      cp <- generate $ injectNegatives injector maxNegative
      res <- evalPathScript solverTimeout cp inputRange solverMaxSeqLength avoidOverflows
      outputSMTProblem res
      putStrLn "INFO: end of smt problem"
  putStrLn "INFO: terminated"
  where
    relevantPaths :: Int -> [SimplePath] -> ([SimplePath],[SimplePath])
    relevantPaths n = foldr f ([],[]) where
      f p (xs,ys)
        | baseDepth == n = (p:xs,ys)
        | maxNegative > 0 && baseDepth + maxNegative== n = (xs,p:ys)
        | otherwise = (xs,ys)
        where
          baseDepth = pathDepth $ completePath [] p

outputSMTProblem :: (SatResult [String], String) -> IO ()
outputSMTProblem (res, code) = do
  putStrLn code
  putStr "result: "
  putStrLn $ case res of
    SAT i -> unlines ["sat", "input: " ++ show i]
    NotSAT -> "unsat"
    Timeout -> "timeout"

runIO :: SessionState a -> IO ()
runIO st = do
  void $ timeout (minutes 1) $
    abortableWith '~' (ioProgram st) `catch` (\(SomeException e) -> Nothing <$ putStrLn (clean (tempPath st) $ displayException e))
  putStrLn "INFO: terminated"

runSpec :: SessionState a -> IO ()
runSpec st =
  let
    action = do
      inputs <- readLn @[String]
      let (trace,warn) = runSpecification (specification st) inputs
      when (warn == OverflowOccurred) $
        putStrLn "INFO: Overflow of Int range detected"
      print (showTraceN $ normalizedTrace trace)
      putStrLn "INFO: terminated"
  in action `catch`(\(SomeException e) -> putStrLn (clean (tempPath st) $ displayException e))

type ProgramSrc = String
type ErrorMsg = String
type Program = IO ()

tryCompile :: forall a. ContextType a -> ProgramSrc -> IO (Either ErrorMsg (Program, Specification, MonitoredIO (), ContextArgs a, FilePath))
tryCompile ctx p = do
  withTempFile "/tmp" "Main.hs" $ \temp tempH -> do
    hPutStr tempH (context ctx p)
    hClose tempH -- close file handle so GHC can load the file later

    logRef <- newIORef ""
    defaultErrorHandler (\s -> putStrLn s >> putStrLn "INFO: failure") defaultFlushOut $
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let
          dflags' = dflags
            { backend = Interpreter
            , ghcLink = LinkInMemory
            }
        pushLogHookM $ \_ LogFlags{log_default_user_context = docContext} msgClass srcSpan doc -> do
            caretDiagnostic <- getCaretDiagnostic msgClass srcSpan
            let errStr = renderWithContext (docContext{sdocStyle = setStyleColoured True (sdocStyle docContext)}) (mkLocMessageAnn Nothing msgClass srcSpan doc $+$ caretDiagnostic)
            modifyIORef logRef (\l -> l <> "\n" <> clean temp errStr)
        setSessionDynFlags dflags'
        addTarget =<< guessTarget temp Nothing Nothing
        load LoadAllTargets


        msg <- liftIO $ readIORef logRef
        if msg == "" -- no error hopefully means Main is loaded successfully
          then do
            setContext
              [importModule "Main"
              ,importModule "System.IO"
              ,importModule "Prelude"
              ,importModule "Test.IOTasks"
              ,importModule "MonitoredIO"
              ]
            (mainPlain, spec, program, args) <- unsafeCoerce @HValue @(IO (), Specification, MonitoredIO (), ContextArgs a) <$> compileExpr "(main, specification, Test.IOTasks.hSetBuffering stdout NoBuffering >> program @MonitoredIO, args)"
            msg <- liftIO $ readIORef logRef
            let
              main = mainPlain `catch` (\(SomeException e) -> rethrowCleaned temp e)
            if msg == "" then pure $ Right (main, spec, program, args,temp) else pure $ Left msg
          else do
            pure $ Left msg

rethrowCleaned :: forall a e. Exception e => FilePath -> e -> IO a
rethrowCleaned temp = throw . CleanedException (clean temp) . toException

clean :: FilePath -> String -> String -- act as if the temp file is called main
clean temp = unpack . replace (pack temp) "Main.hs" . pack

data CleanedException = forall e. Exception e => CleanedException (String -> String) e

instance Show CleanedException where
  show (CleanedException _ e) = "cleaned up version of: " ++ show e ++ "\n (use displayException for the cleaned version)"

instance Exception CleanedException where
  displayException (CleanedException clean e) = clean $ displayException e

importModule :: String -> InteractiveImport
importModule = IIDecl . simpleImportDecl . mkModuleName

ghcVersion :: IO String
ghcVersion =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      showGhcNameVersion . ghcNameVersion <$> getSessionDynFlags

showGhcNameVersion :: GhcNameVersion -> String
showGhcNameVersion (GhcNameVersion ghc ver) = unwords [ghc,ver]
