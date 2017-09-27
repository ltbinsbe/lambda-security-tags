
module Main where

import Parser (parser, lexerSettings) 
import Printer
import Semantics.Static (typings)

import GLL.Combinators (lexer)

import Control.Monad (forM_)

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
        putStrLn "AST:"
        putStrLn (show pr)
        putStrLn "Types:"
        forM_ (typings pr) (putStrLn . show)
