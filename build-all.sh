if [ ! -d "./gen" ]
then
  mkdir "./gen"
fi

if [ ! -d "./gen/Parsing" ]
then
  mkdir "./gen/Parsing"
fi

alex  -g   "./res/Parsing/Lexer.x"  -o "./gen/Parsing/Lexer.hs"
happy -gac "./res/Parsing/Parser.y" -o "./gen/Parsing/Parser.hs"

stack build --exec "parser-exe -f ./examples/run.kt -p -i"
