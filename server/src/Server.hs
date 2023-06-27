{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
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

runMain :: IO ()
runMain = do
  request <- getLine
  case request of
    "get_info" -> getServerInfo
    "send_src" -> do
      context <- getLine
      case context of
        "constraints" -> readSourceAndRun constraintContext
        "random" -> readSourceAndRun randomContext
        _ -> putStrLn "unknown context"
    _ -> putStrLn "invalid request"

readSourceAndRun :: Context -> IO ()
readSourceAndRun ctx = do
  hSetBuffering stdout NoBuffering
  src <- fmap ($ "") loop
  putStrLn "compiling ..."
  catch (compileAndRun ctx src) $
    \(SomeException e) -> putStrLn $ displayException e
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

compileAndRun :: Context -> ProgramSrc -> IO ()
compileAndRun ctx src = do
  res <- tryCompile ctx src
  case res of
    Left msg -> putStrLn msg
    Right p -> p

type ProgramSrc = String
type ErrorMsg = String
type Program = IO ()

tryCompile :: Context -> ProgramSrc -> IO (Either ErrorMsg Program)
tryCompile addContext p = do
  withTempFile "/tmp" "Main.hs" $ \temp tempH -> do
    hPutStr tempH (addContext p)
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
            mainProgram <- unsafeCoerce @HValue @(IO ()) <$> compileExpr "main"
            msg <- liftIO $ readIORef logRef
            if msg == "" then pure $ Right (catch @SomeException mainProgram (rethrowCleaned temp)) else pure $ Left msg
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

type Context = ProgramSrc -> ProgramSrc

constraintContext :: Context
constraintContext = context ConstraintContext

randomContext :: Context
randomContext = context RandomContext

data ContextType = ConstraintContext | RandomContext

ioTasksImport :: ContextType -> String
ioTasksImport ConstraintContext = "IOTasks"
ioTasksImport RandomContext = "IOTasks.Random"

context :: ContextType -> Context
context ctxTy p =
  unlines
    ["{-# LANGUAGE TypeApplications #-}"
    ,"module Main where"
    ,"import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn, until)"
    ,"import " ++ ioTasksImport ctxTy
    ,"import qualified System.IO as SIO"
    ,"import Control.Concurrent"
    ,"main :: IO ()"
    ,"main = do"
    ,"  print =<< getNumCapabilities"
    ,"  SIO.hSetBuffering SIO.stdout SIO.NoBuffering >> taskCheckWith args program specification"
    ]
  <> p

ghcVersion :: IO String
ghcVersion =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      showGhcNameVersion . ghcNameVersion <$> getSessionDynFlags

showGhcNameVersion :: GhcNameVersion -> String
showGhcNameVersion (GhcNameVersion ghc ver) = unwords [ghc,ver]
