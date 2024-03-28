import stylus

proc dumpTokens*(src: string) {.inline.} =
  let tokenizer = newTokenizer(src)

  while not tokenizer.isEof():
    echo repr tokenizer.nextToken()
