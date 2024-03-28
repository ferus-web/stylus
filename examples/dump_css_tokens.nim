## This is a simple example on how to use Stylus' tokenizer.
import std/os, stylus

proc main =
  if paramCount() < 1:
    quit "Usage: dump_css_tokens [filename]"

  let file = paramStr(1)

  if not fileExists(file):
    quit "File not found: " & file

  let 
    data = readFile(file)
    tokenizer = newTokenizer(data)

  echo "Source code:\n" & data
  echo "Tokenized representation:"
  while not tokenizer.isEof():
    echo repr tokenizer.nextToken()

when isMainModule:
  main()
