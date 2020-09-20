{-# LANGUAGE DeriveDataTypeable #-}

module Main
 ( main
 ) where

import Options.Applicative
import System.IO           (openFile, IOMode(ReadMode), hGetContents, hClose)

import Parsing.ParseInput (parseInputStr, parseInputExe)

main :: IO ()
main = runner =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Interpret and format target kotlin file"
     <> header "Kotlin eDSL v1.0"
      )

runner :: Options -> IO ()
runner options = do
  file <- openFile (oPath options) ReadMode
  code <- hGetContents file
  if oPrint options
  then do
    putStrLn "Code:"
    putStrLn $ parseInputStr code
  else return ()
  if oInterpret options
  then do
    putStrLn "Evaluation:"
    parseInputExe code
  else return ()
  hClose file

data Options = Options
  { oPath      :: String
  , oInterpret :: Bool
  , oPrint     :: Bool
  }

sample :: Parser Options
sample = Options
  <$> strOption
      ( long "file"
     <> short 'f'
     <> metavar "FILE"
     <> help "Target file" )
  <*> switch
      ( long "interpret"
     <> short 'i'
     <> help "Print code of the file" )
  <*> switch
      ( long "print"
     <> short 'p'
     <> help "Interpret file" )
