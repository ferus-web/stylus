import stylus, pretty

proc dumpTokens*(src: string) {.inline.} =
  let tokenizer = newTokenizer(src)

  while not tokenizer.isEof():
    print tokenizer.nextToken()
