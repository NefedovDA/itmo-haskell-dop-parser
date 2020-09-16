{-# LANGUAGE InstanceSigs #-}

module Kotlin.TestTemplate
  ( TestTemplate(..)
  
  , HandleIO(..)

  , testTemplates
  
  , testInputFile
  , testOutputFile
  ) where

import Parsing.KotlinPsi
import Kotlin.Interpret (interpret)
import System.IO (hFlush, stdout, IOMode(..), openFile, hPutStrLn, hPutStr, hGetLine, hClose)
import Foreign.Marshal.Unsafe (unsafeLocalState)

testInputFile :: String
testInputFile = "./test-tmp-input.txt"

testOutputFile :: String
testOutputFile = "./test-tmp-output.txt"

data TestTemplate = TestTemplate
  { ttName        :: String
  , ttPsi         :: KotlinPsi (KtFile HandleIO)
  , ttPrinted     :: String
  , ttInterpreted :: Maybe String
  }

data InterpretResult
  = ShouldFailed String
  | ShouldPass   String

data HandleIO a = HandleIO { hioIO :: IO a }

instance Functor HandleIO where
  fmap :: (a -> b) -> HandleIO a -> HandleIO b
  fmap f hio = hio { hioIO = f <$> hioIO hio }

instance Applicative HandleIO where
  pure :: a -> HandleIO a
  pure a = HandleIO $ pure a
  
  (<*>) :: HandleIO (a -> b) -> HandleIO a -> HandleIO b
  hioF <*> hioA = HandleIO $ hioIO hioF <*> hioIO hioA

instance Monad HandleIO where
  (>>=) :: HandleIO a -> (a -> HandleIO b) -> HandleIO b
  before >>= afterF = HandleIO $ hioIO before >>= \a -> hioIO $ afterF a

instance Console HandleIO where
  consolePrint :: String -> HandleIO ()
  consolePrint s = HandleIO $ do
    file <- openFile testOutputFile AppendMode
    hPutStr file s
    hClose file
  
  consolePrintln :: String -> HandleIO ()
  consolePrintln s = HandleIO $ do
    file <- openFile testOutputFile AppendMode
    hPutStrLn file s
    hClose file
  
  consoleReadLine :: HandleIO String
  consoleReadLine = HandleIO $ do
    file <- openFile testOutputFile ReadMode
    s <- hGetLine file
    hClose file
    return s
  

testTemplates :: [TestTemplate]
testTemplates =
  [ TestTemplate
      { ttName = "Empty file"
      , ttPsi = KtPsiFile emptyDeclarations
      , ttPrinted = ""
      , ttInterpreted = Nothing
      }
  , TestTemplate
      { ttName = "Simple main"
      , ttPsi = KtPsiFile $
          emptyDeclarations
            { kdFun0 = [ mainPsi [] ] }
      , ttPrinted =
          "fun main(): Unit {" ++!
          "}"                  ++!
          ""
      , ttInterpreted = Nothing
      }
  , TestTemplate
      { ttName = "Main with return"
      , ttPsi = KtPsiFile $
          emptyDeclarations
            { kdFun0 = [ mainPsi [ KtPsiReturn $ KtPsiUnit () ] ] }
      , ttPrinted =
          "fun main(): Unit {" ++!
          "  return Unit;"     ++!
          "}"                  ++!
          ""
      , ttInterpreted = Nothing
      }
  , TestTemplate
      { ttName = "Main with several return"
      , ttPsi = KtPsiFile $
          emptyDeclarations
            { kdFun0 =
                [ mainPsi
                    [ KtPsiReturn $ KtPsiUnit ()
                    , KtPsiReturn $ KtPsiUnit ()
                    , KtPsiReturn $ KtPsiUnit ()
                    ]
                ]
            }
      , ttPrinted =
          "fun main(): Unit {" ++!
          "  return Unit;"     ++!
          "  return Unit;"     ++!
          "  return Unit;"     ++!
          "}"                  ++!
          ""
      , ttInterpreted = Nothing
      }
  , TestTemplate
      { ttName = "Print statements"
      , ttPsi = KtPsiFile $
          emptyDeclarations
            { kdFun0 =
                [ mainPsi
                    [ printlnPsi $ KtPsiInt    1
                    , printlnPsi $ KtPsiDouble 1.0
                    , printlnPsi $ KtPsiString "s"
                    , printlnPsi $ KtPsiBool   True
                    , printlnPsi $ KtPsiBool   False
                    , printlnPsi $ KtPsiUnit   ()
                    ]
                ]
            }
      , ttPrinted =
          "fun main(): Unit {" ++!
          "  println(1);"      ++!
          "  println(1.0);"    ++!
          "  println(\"s\");"  ++!
          "  println(true);"   ++!
          "  println(false);"  ++!
          "  println(Unit);"   ++!
          "}"                  ++!
          ""
      , ttInterpreted = Just $
          "1"           ++!
          "1.0"         ++!
          "s"           ++!
          "true"        ++!
          "false"       ++!
          "kotlin.Unit" ++!
          ""
      }
  ]

mainPsi :: (Console c) => [KotlinPsi (KtCommand c)] -> KotlinPsi (KtFunData (KtFun0 c))
mainPsi = KtPsiFun0 "main" (KtAnyType KtUnitType)

printlnPsi :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
printlnPsi v = KtPsiValueCommand $ KtPsiCallFun1 "println" v

infixr 5  ++!
(++!) :: String -> String -> String
prefix ++! suffix = prefix ++ "\n" ++ suffix

emptyDeclarations :: KtDeclarations KotlinPsi c
emptyDeclarations =
  KtDeclarations
    { kdFun0 = []
    , kdFun1 = []
    , kdFun2 = []
    }