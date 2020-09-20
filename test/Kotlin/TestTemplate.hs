{-# LANGUAGE InstanceSigs #-}

module Kotlin.TestTemplate
  ( TestTemplate(..)
  , InterpretResult(..)

  , HandleIO(..)

  , testTemplates

  , (.:)
  ) where

import System.IO (IOMode(..), openFile, hPutStrLn, hPutStr, hGetLine, hClose)

import Parsing.KotlinPsi

testInputFile :: String
testInputFile = "./test-tmp-input.txt"

-- | Description of the test data
data TestTemplate expr = TestTemplate
  { ttName        :: String                  -- ^ name
  , ttPsi         :: expr (KtFile HandleIO)  -- ^ psi structure
  , ttPrinted     :: String                  -- ^ expected formatted code
  , ttInterpreted :: InterpretResult         -- ^ expected result of execution
  }

-- | Description a result of the test run
data InterpretResult = InterpretResult
  { irErrors :: String  -- ^ expected error message
  , irOutput :: String  -- ^ expected output
  }

emptyResult = InterpretResult
  { irErrors = ""
  , irOutput = ""
  }

-- | IO wrapper to transfer output to the file.
data HandleIO a = HandleIO
  { hioIO
      :: FilePath  -- ^ Path to the output file
      -> IO a      -- ^ Real IO
  }

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

-- | List of the test templates.
testTemplates :: (Kotlin expr) => [TestTemplate expr]
testTemplates =
  [ TestTemplate
      { ttName = "Empty file"
      , ttPsi = ktFile []
      , ttPrinted = ""
      , ttInterpreted = emptyResult
          { irErrors = "INTERPRET ERROR: No funnction `main()` to call" }
      }
  , TestTemplate
      { ttName = "Simple main"
      , ttPsi = ktFile $
          [ mainPsi [] ]
      , ttPrinted =
          "fun main(): Unit {" ++!
          "}"                  ++!
          ""
      , ttInterpreted = emptyResult
      }
  , TestTemplate
      { ttName = "Main with return"
      , ttPsi = ktFile $
          [ mainPsi [ ktReturn $ ktUnit () ] ]
      , ttPrinted =
          "fun main(): Unit {" ++!
          "  return Unit;"     ++!
          "}"                  ++!
          ""
      , ttInterpreted = emptyResult
      }
  , TestTemplate
      { ttName = "Main with several return"
      , ttPsi = ktFile $
          [ mainPsi
              [ ktReturn $ ktUnit ()
              , ktReturn $ ktUnit ()
              , ktReturn $ ktUnit ()
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
      , ttPsi = ktFile $
          [ mainPsi
              [ printlnPsi $ ktInt    1
              , printlnPsi $ ktDouble 1.0
              , printlnPsi $ ktString "s"
              , printlnPsi $ ktBool   True
              , printlnPsi $ ktBool   False
              , printlnPsi $ ktUnit   ()
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
      , ttInterpreted = emptyResult
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
      , ttPsi = ktFile $
          [ unitFunPsi "f0"
              [ printlnPsi $ ktString "from f0():" ]
          , mainPsi
              [ printlnPsi $ ktString "from main():"
              , callFunPsi "f0" []
              , callFunPsi "f1" [ktInt 1]
              , callFunPsi "f1" [ktBool True]
              , callFunPsi "f2" [ktInt 1, ktInt 2]
              ]
          , ktFun "f1"
              [ "a" .: KtAnyType KtIntType ]
              (KtAnyType KtUnitType)
              [ printlnPsi $ ktString "from f1(Int):"
              , printPsi   $ ktString "  a: "
              , printlnPsi $ ktReadVariable "a"
              ]
          , ktFun "f1"
              [ "a" .: KtAnyType KtBoolType ]
              (KtAnyType KtUnitType)
              [ printlnPsi $ ktString "from f1(Bool):"
              , printPsi   $ ktString "  a: "
              , printlnPsi $ ktReadVariable "a"
              ]
          , ktFun "f2"
              [ "a1" .: KtAnyType KtIntType
              , "a2" .: KtAnyType KtIntType
              ]
              (KtAnyType KtUnitType)
              [ printlnPsi $ ktString "from f2(Int, Int):"
              , printPsi   $ ktString "  a1: "
              , printlnPsi $ ktReadVariable "a1"
              , printPsi   $ ktString "  a2: "
              , printlnPsi $ ktReadVariable "a2"
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
      , ttInterpreted = emptyResult
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
      , ttPsi = ktFile $
          [ mainPsi
              [ valPsi "s" (KtAnyType KtStringType) $
                  ktString "left>" @+@ ktString "<right"
              , printlnPsi $ ktReadVariable "s"
              , varPsi "i" (KtAnyType KtIntType) $ ktInt 1
              , printlnPsi $ ktReadVariable "i"
              , ktSetVariable "i" $
                  ktReadVariable "i" @+@
                  ktReadVariable "i" @+@
                  ktReadVariable "i"
              , printlnPsi $ ktReadVariable "i"
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
      , ttInterpreted = emptyResult
          { irOutput =
            "left><right" ++!
            "1"           ++!
            "3"           ++!
            ""
          }
      }
  , TestTemplate
      { ttName = "For & If"
      , ttPsi = ktFile $
          [ mainPsi
              [ ktFor "i" (ktInt 1) (ktInt 10)
                  [ printPsi $ ktReadVariable "i"
                  , printPsi $ ktString " is "
                  , ktIf
                      [ ( ktReadVariable "i" @/@ ktInt 2 @*@ ktInt 2
                            @==@ ktReadVariable "i"
                        , [ printlnPsi $ ktString "even" ]
                        )
                      ]
                      [ printlnPsi $ ktString "odd" ]
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
      , ttInterpreted = emptyResult
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
  , TestTemplate
      { ttName = "Variable areas"
      , ttPsi = ktFile $
          [ mainPsi
              [ valPsi "i" (KtAnyType KtIntType) $ ktInt 1
              , valPsi "a" (KtAnyType KtIntType) $ ktInt 0
              , scope
                  [ printlnPsi $ ktReadVariable "i"
                  , printlnPsi $ ktReadVariable "a"
                  , valPsi "i" (KtAnyType KtStringType) $ ktString "sss"
                  , scope
                      [ printlnPsi $ ktReadVariable "i"
                      , printlnPsi $ ktReadVariable "a"
                      ]
                  ]
              ]
          ]
      , ttPrinted =
          "fun main(): Unit {"           ++!
          "  val i: Int = 1;"            ++!
          "  val a: Int = 0;"            ++!
          "  if (true) {"                ++!
          "    println(i);"              ++!
          "    println(a);"              ++!
          "    val i: String = \"sss\";" ++!
          "    if (true) {"              ++!
          "      println(i);"            ++!
          "      println(a);"            ++!
          "    }"                        ++!
          "    else {"                   ++!
          "    }"                        ++!
          "  }"                          ++!
          "  else {"                     ++!
          "  }"                          ++!
          "}"                            ++!
          ""
      , ttInterpreted = emptyResult
          { irOutput =
              "1"   ++!
              "0"   ++!
              "sss" ++!
              "0"   ++!
              ""
          }
      }
  , TestTemplate
      { ttName = "Bad access"
      , ttPsi = ktFile $
          [ unitFunPsi "f"
              [ printlnPsi $ ktReadVariable "x" ]
          , mainPsi
              [ valPsi "x" (KtAnyType KtIntType) $ ktInt 10
              , callFunPsi "f" []
              ]
          ]
      , ttPrinted =
          "fun f(): Unit {"    ++!
          "  println(x);"      ++!
          "}"                  ++!
          ""                   ++!
          "fun main(): Unit {" ++!
          "  val x: Int = 10;" ++!
          "  f();"             ++!
          "}"                  ++!
          ""
      , ttInterpreted = emptyResult
          { irErrors = "INTERPRET ERROR: Variable `x` isn't defined." }
      }
  , TestTemplate
      { ttName = "Changing variables"
      , ttPsi = ktFile $
          [ mainPsi
              [ varPsi "x" (KtAnyType KtIntType) $ ktInt 0
              , printlnPsi $ ktReadVariable "x"
              , ktSetVariable "x" $ ktInt 5
              , printlnPsi $ ktReadVariable "x"
              , ktFor "i" (ktInt 1) (ktInt 5)
                  [ ktSetVariable "x" $ ktReadVariable "x" @+@ ktInt 1
                  , printlnPsi $ ktReadVariable "x"
                  ]
              , printlnPsi $ ktReadVariable "x"
              ]
          ]
      , ttPrinted =
          "fun main(): Unit {"  ++!
          "  var x: Int = 0;"   ++!
          "  println(x);"       ++!
          "  x = 5;"            ++!
          "  println(x);"       ++!
          "  for (i in 1..5) {" ++!
          "    x = (x + 1);"    ++!
          "    println(x);"     ++!
          "  }"                 ++!
          "  println(x);"       ++!
          "}"                   ++!
          ""
      , ttInterpreted = emptyResult
          { irOutput =
              "0"  ++!
              "5"  ++!
              "6"  ++!
              "7"  ++!
              "8"  ++!
              "9"  ++!
              "10" ++!
              "10" ++!
              ""
          }
      }
  ]

scope :: (Kotlin expr, Console c) => [expr (KtCommand c)] -> expr (KtCommand c)
scope cmds = ktIf [ ktBool True .: cmds] []

valPsi
  :: (Kotlin expr, Console c)
  => Name -> KtAnyType -> expr (KtValue c) -> expr (KtCommand c)
valPsi = ktInitVariable True

varPsi
  :: (Kotlin expr, Console c)
  => Name -> KtAnyType -> expr (KtValue c) -> expr (KtCommand c)
varPsi = ktInitVariable False

unitFunPsi :: (Kotlin expr, Console c) => Name -> [expr (KtCommand c)] -> expr (KtFunData c)
unitFunPsi name = ktFun name [] (KtAnyType KtUnitType)

mainPsi :: (Kotlin expr, Console c) => [expr (KtCommand c)] -> expr (KtFunData c)
mainPsi = unitFunPsi "main"

callFunPsi :: (Kotlin expr, Console c) => Name -> [expr (KtValue c)] -> expr (KtCommand c)
callFunPsi n as = ktValueCommand $ ktCallFun n as

printPsi :: (Kotlin expr, Console c) => expr (KtValue c) -> expr (KtCommand c)
printPsi a = callFunPsi "print" [a]

printlnPsi :: (Kotlin expr, Console c) => expr (KtValue c) -> expr (KtCommand c)
printlnPsi a = callFunPsi "println" [a]

infixr 5  ++!
(++!) :: String -> String -> String
prefix ++! suffix = prefix ++ "\n" ++ suffix

-- | Idiomatic binding of (,) to use it as infix operator.
infix 1 .:
(.:) :: a -> b -> (a, b)
(.:) = (,)
