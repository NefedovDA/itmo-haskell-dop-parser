{-# LANGUAGE InstanceSigs #-}

module Kotlin.TestTemplate
  ( TestTemplate(..)
  , InterpretResult(..)

  , HandleIO(..)

  , testTemplates
  ) where

import System.IO (IOMode(..), openFile, hPutStrLn, hPutStr, hGetLine, hClose)

import Kotlin.Language

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
          [ main_ [] ]
      , ttPrinted =
          "fun main(): Unit {" ++!
          "}"                  ++!
          ""
      , ttInterpreted = emptyResult
      }
  , TestTemplate
      { ttName = "Main with return"
      , ttPsi = ktFile $
          [ main_ [ ktReturn $ ktUnit () ] ]
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
          [ main_
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
          [ main_
              [ println_ $ ktInt    1
              , println_ $ ktDouble 1.0
              , println_ $ ktString "s"
              , println_ $ ktBool   True
              , println_ $ ktBool   False
              , println_ $ ktUnit   ()
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
          [ fun "f0" [] _Unit
              [ println_ $ ktString "from f0():" ]
          , main_
              [ println_ $ ktString "from main():"
              , "f0".!#[]
              , "f1".!#[ktInt 1]
              , "f1".!#[ktBool True]
              , "f2".!#[ktInt 1, ktInt 2]
              ]
          , ktFun "f1" ["a" .: _Int] _Unit
              [ println_ $ ktString "from f1(Int):"
              , print_   $ ktString "  a: "
              , println_ $ ktReadVariable "a"
              ]
          , ktFun "f1" ["a" .: _Bool] _Unit
              [ println_ $ ktString "from f1(Bool):"
              , print_   $ ktString "  a: "
              , println_ $ ktReadVariable "a"
              ]
          , ktFun "f2" ["a1" .: _Int, "a2" .: _Int] _Unit
              [ println_ $ ktString "from f2(Int, Int):"
              , print_   $ ktString "  a1: "
              , println_ $ ktReadVariable "a1"
              , print_   $ ktString "  a2: "
              , println_ $ ktReadVariable "a2"
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
          [ main_
              [ val "s" _String .=
                  ktString "left>" @+@ ktString "<right"
              , println_ $ get "s"
              , var "i" _Int .= ktInt 1
              , println_ $ get "i"
              , "i" .=#
                  get "i" @+@
                  get "i" @+@
                  get "i"
              , println_ $ get "i"
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
          [ main_
              [ for ("i" `_in` ktInt 1 ..# ktInt 10)
                  [ print_ $ get "i"
                  , print_ $ ktString " is "
                  , _if (get "i" @/@ ktInt 2 @*@ ktInt 2 @==@ get "i")
                      [ println_ $ ktString "even" ]
                    $ _else_
                      [ println_ $ ktString "odd" ]
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
          [ main_
              [ val "i" _Int .= ktInt 1
              , val "a" _Int .= ktInt 0
              , scope  -- synthetic
                  [ println_ $ get "i"
                  , println_ $ get "a"
                  , val "i" _String .= ktString "sss"
                  , scope  -- synthetic
                      [ println_ $ get "i"
                      , println_ $ get "a"
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
          [ fun "f" [] _Unit
              [ println_ $ ktReadVariable "x" ]
          , main_
              [ val "x" _Int .= ktInt 10
              , "f".!#[]
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
          [ main_
              [ var "x" _Int .= ktInt 0
              , println_ $ get "x"
              , "x" .=# ktInt 5
              , println_ $ get "x"
              , for ("i" `_in` ktInt 1 ..# ktInt 5)
                  [ "x" .=# get "x" @+@ ktInt 1
                  , println_ $ get "x"
                  ]
              , println_ $ get "x"
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
  , TestTemplate
      { ttName = "Check that not lazy"
      , ttPsi = ktFile $
          [ ktFun "f" ["r" .: _Int] _Int
              [ println_ $ get "r"
              , ktReturn $ get "r" @+@ ktInt 1
              ]
          , main_
              [ val "x" _Int .= "f".![ktInt 1]
              , println_ $ get "x"
              , println_ $ "f".!["f".![ktInt 3]]
              , println_ $ "println".![ktBool True]
              ]
          ]
      , ttPrinted =
          "fun f(r: Int): Int {"      ++!
          "  println(r);"             ++!
          "  return (r + 1);"         ++!
          "}"                         ++!
          ""                          ++!
          "fun main(): Unit {"        ++!
          "  val x: Int = f(1);"      ++!
          "  println(x);"             ++!
          "  println(f(f(3)));"       ++!
          "  println(println(true));" ++!
          "}"                         ++!
          ""
      , ttInterpreted = emptyResult
          { irOutput =
              "1"           ++!
              "2"           ++!
              "3"           ++!
              "4"           ++!
              "5"           ++!
              "true"        ++!
              "kotlin.Unit" ++!
              ""
          }
      }
  ]

scope :: (Kotlin expr, Console c) => [expr (KtCommand c)] -> expr (KtCommand c)
scope cmds = _if (ktBool True) cmds $ _else_ []

main_ :: (Kotlin expr, Console c) => [expr (KtCommand c)] -> expr (KtFunData c)
main_ = fun "main" [] _Unit

print_ :: (Kotlin expr, Console c) => expr (KtValue c) -> expr (KtCommand c)
print_ a = "print".!#[a]

println_ :: (Kotlin expr, Console c) => expr (KtValue c) -> expr (KtCommand c)
println_ a = "println".!#[a]

infixr 5  ++!
(++!) :: String -> String -> String
prefix ++! suffix = prefix ++ "\n" ++ suffix
