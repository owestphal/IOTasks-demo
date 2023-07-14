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
import Control.Exception (bracket, catch, throw, Exception(..), SomeException(..))
import Control.Concurrent.Async (race, concurrently)
import Control.Monad (void, forM_, replicateM_)
import Control.Concurrent.STM

import Test.IOTasks as Constraints (Specification, Args(..))
import Test.IOTasks.Random as Random (Args(..), genInput)
import Test.IOTasks.Constraints (paths,constraintTree, pathDepth)
import Test.IOTasks.ValueSet (Size(..))
import Test.IOTasks.Z3 (evalPathScript, SatResult (..), satPathsQ, findPathInput)

import Test.QuickCheck (generate)

import Context

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
    Right (p,s,a) -> do
      putStrLn "INFO: success"
      sessionLoop $ newSession ctx p s a

data SessionState a where
  ConstraintSession :: Specification -> Constraints.Args -> IO () -> SessionState ConstraintType
  RandomSession :: Specification -> Random.Args -> IO () -> SessionState RandomType

newSession :: ContextType a -> IO () -> Specification -> ContextArgs a -> SessionState a
newSession RandomContext = newRandomSession
newSession ConstraintContext = newConstraintSession

newConstraintSession :: IO () -> Specification -> Constraints.Args -> SessionState ConstraintType
newConstraintSession p s a = ConstraintSession s a p

newRandomSession :: IO () -> Specification -> Random.Args -> SessionState RandomType
newRandomSession p s a = RandomSession s a p

mainProgram :: SessionState a -> IO ()
mainProgram (RandomSession _ _ p) = p
mainProgram (ConstraintSession _ _ p) = p

sessionLoop :: forall a. SessionState a -> IO ()
sessionLoop st = case st of
  RandomSession{} -> randomSession
  ConstraintSession{} -> constraintsSession
  where
    randomSession :: (a ~ RandomType) => IO ()
    randomSession = do
      cmd <- getLine
      case cmd of
        "run" -> runProgram (mainProgram st) >> randomSession
        "sample_input" -> sampleInput st >> randomSession
        "exit" -> pure ()
        _ -> error "unknown command"
    constraintsSession :: (a ~ ConstraintType) => IO ()
    constraintsSession = do
      cmd <- getLine
      case cmd of
        "run" -> runProgram (mainProgram st) >> constraintsSession
        "smt_code" -> smtCode st >> constraintsSession
        "sample_input" -> sampleInput st >> constraintsSession
        "exit" -> pure ()
        _ -> error "unknown command"

runProgram :: IO () -> IO ()
runProgram p = do
  abortable p
  putStrLn "INFO: terminated"

abortable :: IO a -> IO ()
abortable p = void $ race waitForAbort $ do
  void p `catch` (\(SomeException e) -> putStrLn $ displayException e)

waitForAbort :: IO ()
waitForAbort = do
  msg <- getLine
  case msg of
    "abort" -> pure ()
    _ -> waitForAbort

data Abort = Abort deriving Show
instance Exception Abort

sampleInput :: SessionState a -> IO ()
sampleInput (RandomSession s Random.Args{..} _ )  = do
  n <- readLn
  abortable $ replicateM_ n $ do
    i <- generate $ Random.genInput s maxInputLength (Size valueSize (fromIntegral $ valueSize `div` 5)) maxNegative
    print i
  putStrLn "INFO: terminated"
sampleInput (ConstraintSession s Constraints.Args{..} _ )  = do
  n <- readLn
  m <- readLn
  abortable $ do
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
            res <- findPathInput solverTimeout p valueSize solverMaxSeqLength checkOverflows
            case res of
              SAT i -> print i >> outputInputs (x-1)
              Timeout -> outputInputs x
              NotSAT -> outputInputs x
          Nothing -> pure ()
    concurrently
      (satPathsQ nVar solverTimeout (constraintTree maxNegative s) maxIterationUnfold solverMaxSeqLength checkOverflows qVar)
      (outputInputs n)
  putStrLn "INFO: terminated"

smtCode :: SessionState ConstraintType -> IO ()
smtCode (ConstraintSession s Constraints.Args{..} _) = do
  n <- readLn
  abortable $ do
    let ps = filter ((== n) . pathDepth) $ paths n $ constraintTree maxNegative s
    forM_ ps $ \p -> do
      res <- evalPathScript solverTimeout p valueSize solverMaxSeqLength checkOverflows
      outputSMTProblem res
      putStrLn "INFO: end of smt problem"
  putStrLn "INFO: terminated"

outputSMTProblem :: (SatResult [String], String) -> IO ()
outputSMTProblem (res, code) = do
  putStrLn code
  putStr "result: "
  putStrLn $ case res of
    SAT i -> unlines ["sat", "input: " ++ show i]
    NotSAT -> "unsat"
    Timeout -> "timeout"

type ProgramSrc = String
type ErrorMsg = String
type Program = IO ()

tryCompile :: forall a. ContextType a -> ProgramSrc -> IO (Either ErrorMsg (Program, Specification, ContextArgs a))
tryCompile ctx p = do
  withTempFile "/tmp" "Main.hs" $ \temp tempH -> do
    hPutStr tempH (context ctx p)
    hClose tempH -- close file handle so GHC can load the file later

    logRef <- newIORef ""
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
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
              ]
            (mainPlain, spec, args) <- unsafeCoerce @HValue @(IO (), Specification, ContextArgs a) <$> compileExpr "(main, specification, args)"
            let mainProgram = catch @SomeException mainPlain (rethrowCleaned temp)
            msg <- liftIO $ readIORef logRef
            if msg == "" then pure $ Right (mainProgram, spec, args) else pure $ Left msg
          else do
            pure $ Left msg
  where
    clean :: FilePath -> String -> String -- act as if the temp file is called main
    clean temp = unpack . replace (pack temp) "Main.hs" . pack
    rethrowCleaned :: Exception e => FilePath -> e -> IO ()
    rethrowCleaned temp = throw . CleanedException (clean temp) . toException

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
