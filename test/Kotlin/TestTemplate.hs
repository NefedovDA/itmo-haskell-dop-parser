{-# LANGUAGE InstanceSigs #-}

module Kotlin.TestTemplate
  ( TestTemplate(..)
  
  , HandleIO(..)

  , testTemplates
  ) where

import Parsing.KotlinPsi
import Kotlin.Interpret (interpret)
import System.IO (hFlush, stdout, IOMode(..), openFile, hPutStrLn, hPutStr, hGetLine, hClose)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Control.Exception (evaluate)

testInputFile :: String
testInputFile = "./test-tmp-input.txt"

data TestTemplate = TestTemplate
  { ttName        :: String
  , ttPsi         :: KotlinPsi (KtFile HandleIO)
  , ttPrinted     :: String
  , ttInterpreted :: Maybe String
  }

data InterpretResult
  = ShouldFailed String
  | ShouldPass   String

data HandleIO a = HandleIO { hioIO :: FilePath -> IO a }

instance Functor HandleIO where
  fmap :: (a -> b) -> HandleIO a -> HandleIO b
  fmap f hio = hio { hioIO = \path -> f <$> hioIO hio path }

instance Applicative HandleIO where
  pure :: a -> HandleIO a
  pure a = HandleIO $ \path -> pure a
  
  (<*>) :: HandleIO (a -> b) -> HandleIO a -> HandleIO b
  hioF <*> hioA = HandleIO $ \path ->
    hioIO hioF path <*> hioIO hioA path

instance Monad HandleIO where
  (>>=) :: HandleIO a -> (a -> HandleIO b) -> HandleIO b
  before >>= afterF = HandleIO $ \path ->
    hioIO before path >>= \a -> hioIO (afterF a) path

instance Console HandleIO where
  consolePrint :: String -> HandleIO ()
  consolePrint s = HandleIO $ \path -> do
    file <- openFile path AppendMode
    hPutStr file s
    hClose file
  
  consolePrintln :: String -> HandleIO ()
  consolePrintln s = HandleIO $ \path -> do
    file <- openFile path AppendMode
    hPutStrLn file s
    hClose file
  
  consoleReadLine :: HandleIO String
  consoleReadLine = HandleIO $ \_ -> do
    file <- openFile testInputFile ReadMode
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
  , TestTemplate
      { ttName = "Call functions"
      , ttPsi = KtPsiFile $
          emptyDeclarations
            { kdFun0 =
                [ unitFunPsi "f0"
                    [ printlnPsi $ KtPsiString "from f0():" ]
                , mainPsi
                    [ printlnPsi $ KtPsiString "from main():"
                    , callFun0Psi "f0"
                    , callFun1Psi "f1" (KtPsiInt 1)
                    , callFun1Psi "f1" (KtPsiBool True)
                    , callFun2Psi "f2" (KtPsiInt 1) (KtPsiInt 2)
                    ]
                ]
            , kdFun1 =
                [ KtPsiFun1 "f1"
                    ("a", KtAnyType KtIntType)
                    (KtAnyType KtUnitType)
                    [ printlnPsi $ KtPsiString "from f1(Int):"
                    , printPsi $ KtPsiString "  a: "
                    , printlnPsi $ KtPsiReadVariable "a"
                    ]
                , KtPsiFun1 "f1"
                    ("a", KtAnyType KtBoolType)
                    (KtAnyType KtUnitType)
                    [ printlnPsi $ KtPsiString "from f1(Bool):"
                    , printPsi $ KtPsiString "  a: "
                    , printlnPsi $ KtPsiReadVariable "a"
                    ]
                ]
            , kdFun2 =
                [ KtPsiFun2 "f2"
                    ("a1", KtAnyType KtIntType)
                    ("a2", KtAnyType KtIntType)
                    (KtAnyType KtUnitType)
                    [ printlnPsi $ KtPsiString "from f2(Int, Int):"
                    , printPsi $ KtPsiString "  a1: "
                    , printlnPsi $ KtPsiReadVariable "a1"
                    , printPsi $ KtPsiString "  a2: "
                    , printlnPsi $ KtPsiReadVariable "a2"
                    ]
                ]
            }
      , ttPrinted =
          "fun f0(): Unit {"                   ++!
          "  println(\"from f0():\");"         ++!
          "}"                                  ++!
          ""                                   ++!
          "fun main(): Unit {"                 ++!
          "  println(\"from main():\");"       ++!
          "  f0();"                            ++!
          "  f1(1);"                           ++!
          "  f1(true);"                        ++!
          "  f2(1, 2);"                        ++!
          "}"                                  ++!
          ""                                   ++!
          "fun f1(a: Int): Unit {"             ++!
          "  println(\"from f1(Int):\");"      ++!
          "  print(\"  a: \");"                ++!
          "  println(a);"                      ++!
          "}"                                  ++!
          ""                                   ++!
          "fun f1(a: Bool): Unit {"            ++!
          "  println(\"from f1(Bool):\");"     ++!
          "  print(\"  a: \");"                ++!
          "  println(a);"                      ++!
          "}"                                  ++!
          ""                                   ++!
          "fun f2(a1: Int, a2: Int): Unit {"   ++!
          "  println(\"from f2(Int, Int):\");" ++!
          "  print(\"  a1: \");"               ++!
          "  println(a1);"                     ++!
          "  print(\"  a2: \");"               ++!
          "  println(a2);"                     ++!
          "}"                                  ++!
          ""
      , ttInterpreted = Just $
          "from main():"       ++!
          "from f0():"         ++!
          "from f1(Int):"      ++!
          "  a: 1"             ++!
          "from f1(Bool):"     ++!
          "  a: true"          ++!
          "from f2(Int, Int):" ++!
          "  a1: 1"            ++!
          "  a2: 2"            ++!
          ""
      }
  , TestTemplate
      { ttName = "Work with variables"
      , ttPsi = KtPsiFile $
          emptyDeclarations
            { kdFun0 =
                [ mainPsi
                    [ valPsi "s" (KtAnyType KtStringType) (KtPsiString "left>" `KtPsiAddition` KtPsiString "<right")
                    , printlnPsi (KtPsiReadVariable "s")
                    , varPsi "i" (KtAnyType KtIntType) (KtPsiInt 1)
                    , printlnPsi (KtPsiReadVariable "i")
                    , KtPsiSetVariable "i"
                        ( KtPsiReadVariable "i" `KtPsiAddition`
                          KtPsiReadVariable "i" `KtPsiAddition` 
                          KtPsiReadVariable "i"
                        )
                    , printlnPsi (KtPsiReadVariable "i")
                    ]
                ]
            }
      , ttPrinted =
          "fun main(): Unit {"             ++!
          "  val s: String = "
            ++ "(\"left>\" + \"<right\");" ++!
          "  println(s);"                  ++!
          "  var i: Int = 1;"              ++!
          "  println(i);"                  ++!
          "  i = ((i + i) + i);"           ++!
          "  println(i);"                  ++!
          "}"                              ++!
          ""
      , ttInterpreted = Just $
          "left><right" ++!
          "1"           ++!
          "3"           ++!
          ""
      }
  ]

v = 1 + 1

valPsi :: (Console c) => Name -> KtAnyType -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
valPsi = KtPsiInitVariable True

varPsi :: (Console c) => Name -> KtAnyType -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
varPsi = KtPsiInitVariable False

unitFunPsi :: (Console c) => Name -> [KotlinPsi (KtCommand c)] -> KotlinPsi (KtFunData (KtFun0 c))
unitFunPsi name = KtPsiFun0 name (KtAnyType KtUnitType)

mainPsi :: (Console c) => [KotlinPsi (KtCommand c)] -> KotlinPsi (KtFunData (KtFun0 c))
mainPsi = unitFunPsi "main"

callFun0Psi :: (Console c) => Name -> KotlinPsi (KtCommand c)
callFun0Psi = KtPsiValueCommand . KtPsiCallFun0

callFun1Psi :: (Console c) => Name -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
callFun1Psi n a = KtPsiValueCommand $ KtPsiCallFun1 n a

callFun2Psi
  :: (Console c)
  => Name -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
callFun2Psi n a1 a2 = KtPsiValueCommand $ KtPsiCallFun2 n a1 a2

printPsi :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
printPsi = callFun1Psi "print"

printlnPsi :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
printlnPsi = callFun1Psi "println"

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