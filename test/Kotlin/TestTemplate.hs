{-# LANGUAGE InstanceSigs #-}

module Kotlin.TestTemplate
  ( TestTemplate(..)
  , InterpretResult(..)
  
  , HandleIO(..)

  , testTemplates
  ) where

import System.IO (hFlush, stdout, IOMode(..), openFile, hPutStrLn, hPutStr, hGetLine, hClose)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Control.Exception (evaluate)

import Parsing.KotlinPsi
import Kotlin.Interpret (interpret)
import Kotlin.Utils (to)

testInputFile :: String
testInputFile = "./test-tmp-input.txt"

data TestTemplate = TestTemplate
  { ttName        :: String
  , ttPsi         :: KotlinPsi (KtFile HandleIO)
  , ttPrinted     :: String
  , ttInterpreted :: InterpretResult
  }

data InterpretResult = InterpretResult
  { irErrors :: String
  , irOutput :: String
  }

emptyResult = InterpretResult
  { irErrors = ""
  , irOutput = ""
  }

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
      , ttPsi = KtPsiFile []
      , ttPrinted = ""
      , ttInterpreted =
          emptyResult
            { irErrors = "INTERPRET ERROR: No funnction `main()` to call" }
      }
  , TestTemplate
      { ttName = "Simple main"
      , ttPsi = KtPsiFile $
          [ mainPsi [] ]
      , ttPrinted =
          "fun main(): Unit {" ++!
          "}"                  ++!
          ""
      , ttInterpreted = emptyResult
      }
  , TestTemplate
      { ttName = "Main with return"
      , ttPsi = KtPsiFile $
          [ mainPsi [ KtPsiReturn $ KtPsiUnit () ] ]
      , ttPrinted =
          "fun main(): Unit {" ++!
          "  return Unit;"     ++!
          "}"                  ++!
          ""
      , ttInterpreted = emptyResult
      }
  , TestTemplate
      { ttName = "Main with several return"
      , ttPsi = KtPsiFile $
          [ mainPsi
              [ KtPsiReturn $ KtPsiUnit ()
              , KtPsiReturn $ KtPsiUnit ()
              , KtPsiReturn $ KtPsiUnit ()
              ]
          ]
      , ttPrinted =
          "fun main(): Unit {" ++!
          "  return Unit;"     ++!
          "  return Unit;"     ++!
          "  return Unit;"     ++!
          "}"                  ++!
          ""
      , ttInterpreted = emptyResult
      }
  , TestTemplate
      { ttName = "Print statements"
      , ttPsi = KtPsiFile $
          [ mainPsi
              [ printlnPsi $ KtPsiInt    1
              , printlnPsi $ KtPsiDouble 1.0
              , printlnPsi $ KtPsiString "s"
              , printlnPsi $ KtPsiBool   True
              , printlnPsi $ KtPsiBool   False
              , printlnPsi $ KtPsiUnit   ()
              ]
          ]
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
      , ttInterpreted =
          emptyResult
            { irOutput =
                "1"           ++!
                "1.0"         ++!
                "s"           ++!
                "true"        ++!
                "false"       ++!
                "kotlin.Unit" ++!
                ""
          }
      }
  , TestTemplate
      { ttName = "Call functions"
      , ttPsi = KtPsiFile $
          [ unitFunPsi "f0"
              [ printlnPsi $ psi's "from f0():" ]
          , mainPsi
              [ printlnPsi $ psi's "from main():"
              , callFun0Psi "f0"
              , callFun1Psi "f1" psi'1
              , callFun1Psi "f1" (KtPsiBool True)
              , callFun2Psi "f2" psi'1 psi'2
              ]
          , KtPsiFun "f1"
              [ "a" `to` KtAnyType KtIntType ]
              (KtAnyType KtUnitType)
              [ printlnPsi $ psi's "from f1(Int):"
              , printPsi   $ psi's "  a: "
              , printlnPsi $ KtPsiReadVariable "a"
              ]
          , KtPsiFun "f1"
              [ "a" `to` KtAnyType KtBoolType ]
              (KtAnyType KtUnitType)
              [ printlnPsi $ psi's "from f1(Bool):"
              , printPsi   $ psi's "  a: "
              , printlnPsi $ KtPsiReadVariable "a"
              ]
          , KtPsiFun "f2"
              [ "a1" `to` KtAnyType KtIntType
              , "a2" `to` KtAnyType KtIntType
              ]
              (KtAnyType KtUnitType)
              [ printlnPsi $ psi's "from f2(Int, Int):"
              , printPsi   $ psi's "  a1: "
              , printlnPsi $ KtPsiReadVariable "a1"
              , printPsi   $ psi's "  a2: "
              , printlnPsi $ KtPsiReadVariable "a2"
              ]
          ]
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
      , ttInterpreted =
          emptyResult
            { irOutput =
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
      }
  , TestTemplate
      { ttName = "Work with variables"
      , ttPsi = KtPsiFile $
          [ mainPsi
              [ valPsi "s" (KtAnyType KtStringType) (psi's "left>" :+: psi's "<right")
              , printlnPsi (KtPsiReadVariable "s")
              , varPsi "i" (KtAnyType KtIntType) psi'1
              , printlnPsi (KtPsiReadVariable "i")
              , KtPsiSetVariable "i"
                  ( KtPsiReadVariable "i" :+:
                    KtPsiReadVariable "i" :+:
                    KtPsiReadVariable "i"
                  )
              , printlnPsi (KtPsiReadVariable "i")
              ]
          ]
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
      , ttInterpreted =
          emptyResult
            { irOutput =
              "left><right" ++!
              "1"           ++!
              "3"           ++!
              ""
            }
      }
  , TestTemplate
      { ttName = "For & If"
      , ttPsi = KtPsiFile $
          [ mainPsi
              [ KtPsiFor "i" psi'1 (KtPsiInt 10)
                  [ printPsi (KtPsiReadVariable "i")
                  , printPsi (psi's " is ")
                  , KtPsiIf
                      [ ( KtPsiReadVariable "i" :/: psi'2 :*: psi'2
                            :==: KtPsiReadVariable "i"
                        , [ printlnPsi (psi's "even") ]
                        )
                      ]
                      [ printlnPsi (psi's "odd") ]
                  ]
              ]
          ]
      , ttPrinted =
          "fun main(): Unit {"              ++!
          "  for (i in 1..10) {"            ++!
          "    print(i);"                   ++!
          "    print(\" is \");"            ++!
          "    if ((((i / 2) * 2) == i)) {" ++!
          "      println(\"even\");"        ++!
          "    }"                           ++!
          "    else {"                      ++!
          "      println(\"odd\");"         ++!
          "    }"                           ++!
          "  }"                             ++!
          "}"                               ++!
          ""
      , ttInterpreted =
          emptyResult
            { irOutput =
                "1 is odd"   ++!
                "2 is even"  ++!
                "3 is odd"   ++!
                "4 is even"  ++!
                "5 is odd"   ++!
                "6 is even"  ++!
                "7 is odd"   ++!
                "8 is even"  ++!
                "9 is odd"   ++!
                "10 is even" ++!
                ""
            }
      }
  ]

psi'1 :: (Console c) => KotlinPsi (KtValue c)
psi'1 = KtPsiInt 1

psi'2 :: (Console c) => KotlinPsi (KtValue c)
psi'2 = KtPsiInt 2

psi's :: (Console c) => String -> KotlinPsi (KtValue c)
psi's = KtPsiString

valPsi :: (Console c) => Name -> KtAnyType -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
valPsi = KtPsiInitVariable True

varPsi :: (Console c) => Name -> KtAnyType -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
varPsi = KtPsiInitVariable False

unitFunPsi :: (Console c) => Name -> [KotlinPsi (KtCommand c)] -> KotlinPsi (KtFunData c)
unitFunPsi name = KtPsiFun name [] (KtAnyType KtUnitType)

mainPsi :: (Console c) => [KotlinPsi (KtCommand c)] -> KotlinPsi (KtFunData c)
mainPsi = unitFunPsi "main"

callFun0Psi :: (Console c) => Name -> KotlinPsi (KtCommand c)
callFun0Psi n = KtPsiValueCommand $ KtPsiCallFun n []

callFun1Psi :: (Console c) => Name -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
callFun1Psi n a = KtPsiValueCommand $ KtPsiCallFun n [a]

callFun2Psi
  :: (Console c)
  => Name -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
callFun2Psi n a1 a2 = KtPsiValueCommand $ KtPsiCallFun n [a1, a2]

printPsi :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
printPsi = callFun1Psi "print"

printlnPsi :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
printlnPsi = callFun1Psi "println"

infixr 5  ++!
(++!) :: String -> String -> String
prefix ++! suffix = prefix ++ "\n" ++ suffix
