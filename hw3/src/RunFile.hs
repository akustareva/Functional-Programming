{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RunFile
       ( main
       ) where

import           ConsoleInput
import           ConsoleOutput
import           Control.Applicative    (pure, (<*>))
import           Control.Monad          (when)
import           Control.Monad.Except   (MonadError, runExceptT)
import           Control.Monad.Reader   (MonadIO, liftIO)
import           Control.Monad.State    (MonadState, evalStateT, runStateT)
import           CustomError
import           Data.Map               (Map, empty)
import           FullValueAssignment
import           System.Environment     (getArgs)
import           Text.Megaparsec        (parseMaybe)
import           VariableCreationParser

main :: IO ()
main = do
       args <- getArgs
       when (length args /= 1) $ putStrLn "Please specify filename"
       when (length args == 1) $ do
                                 s <- readFile (head args)
                                 runExceptT $ evalStateT (processLines (lines s)) empty
                                 return ()

processLines :: ( MonadState (Map String Integer)  m
                , MonadError CustomError m
                , MonadIO m
                )
             => [String] -> m ()
processLines []     = return ()
processLines (l:ls) = do
                      let actM = parseMaybe actionParser l
                      case actM of
                          Just act -> updateVarsSet [act]
                          Nothing  -> do
                                      let outM = parseMaybe writeStmtParser l
                                      case outM of
                                          Just out -> printResult out
                                          Nothing  -> do
                                                      let inM = parseMaybe readStmtParser l
                                                      case inM of
                                                          Just in' -> readStmt in'
                                                          Nothing  -> liftIO $ print ("Coudn't parse " ++ l)
                      processLines ls
