resRoot=./res
genRoot=./gen

genModule=Parsing

if [ ! -d "${genRoot}/${genModule}" ]
then
  mkdir "${genRoot}/${genModule}"
fi

alex -g "${resRoot}/${genModule}/Lexer.x" -o "${genRoot}/${genModule}/Lexer.hs"

stack build --exec parser-exe