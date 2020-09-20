{-# LANGUAGE DeriveDataTypeable #-}

module Main
 ( main
 ) where

import           Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as O
import           System.IO           (openFile, IOMode(ReadMode), hGetContents, hClose)

import Parsing.ParseInput (parseInputStr, parseInputExe)

-- | Entry point of the application
main :: IO ()
main = runner =<< O.execParser opts
  where
    -- | Specify description of the options.
    opts = O.info (options <**> O.helper)
      ( O.fullDesc
     <> O.progDesc "Interpret and format target kotlin file"
     <> O.header "Kotlin eDSL v1.0"
      )

-- | Run the specified by given options actions.
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

-- | Data type for console options.
data Options = Options
  { oPath      :: String  -- ^ Path to the target file.
  , oInterpret :: Bool    -- ^ Set if we should interpret the target file.
  , oPrint     :: Bool    -- ^ Set if we should print the formatted target file.
  }

-- | Specify how should be presented options.
options :: Parser Options
options = Options
  <$> O.strOption
      ( O.long "file"
     <> O.short 'f'
     <> O.metavar "FILE"
     <> O.help "Target file" )
  <*> O.switch
      ( O.long "interpret"
     <> O.short 'i'
     <> O.help "Print code of the file" )
  <*> O.switch
      ( O.long "print"
     <> O.short 'p'
     <> O.help "Interpret file" )
