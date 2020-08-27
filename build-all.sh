resRoot=./res
genRoot=./gen

genModule=Parsing

if [ ! -d "${genRoot}/${genModule}" ]
then
  mkdir "${genRoot}/${genModule}"
fi

alex  -g   "${resRoot}/${genModule}/Lexer.x"  -o "${genRoot}/${genModule}/Lexer.hs"
happy -gac "${resRoot}/${genModule}/Parser.y" -o "${genRoot}/${genModule}/Parser.hs"

stack build --exec parser-exe