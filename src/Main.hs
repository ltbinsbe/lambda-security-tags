
module Main where

import Parser (parser, lexerSettings) 
import Printer
import Semantics.Static (typings)
import Semantics.Dynamic (eval)

import GLL.Combinators (lexer)

import Control.Monad (forM_, when, unless)

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of 
    f:args  -> go args f 
    []      -> putStrLn "Please provide an input file"

go :: [String] -> FilePath -> IO ()
go args f = do
  file <- readFile f
  let eprograms = parser (lexer lexerSettings file)
  forM_ (zip [1..] eprograms) $ \(i,mpr) -> do 
    putStrLn ("== Interpretation " ++ show i ++ " ==")
    case mpr of 
      Left err  -> putStrLn ("error: " ++ err)
      Right pr  -> do
        putStrLn (show pr)
        putStrLn "=="
        let types = typings pr
        when (null types) (putStrLn "Program does not type-check")
        unless (null types) $ do
          putStrLn "Types:"
          forM_ types (putStrLn . show)
          putStrLn "Evaluation result:"
          putStrLn (show (eval pr))

