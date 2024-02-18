import std/[options, strutils, math],
       shared, results

proc unpack[T](opt: Option[T], x: var T): bool {.inline.} =
  # thanks, araq. very cool. :D
  if opt.isSome:
    x = unsafeGet(opt)
    return true

  false

type
  TokenizerDefect* = object of Defect
  Tokenizer* = ref object
    input: string
    pos: uint
    currLineStartPos: uint
    currLineNumber: uint32
    varOrEnvFunctions: SeenStatus
    sourceMapUrl, sourceUrl: Option[string]

  QuotedString* = Result[string, string]

proc tokenizer*(input: string): Tokenizer {.inline.} =
  Tokenizer(
    input: input,
    pos: 0'u,
    currLineStartPos: 0'u,
    currLineNumber: 0'u32,
    varOrEnvFunctions: ssDontCare
  )

proc lookForVarOrEnvFunctions*(tokenizer: Tokenizer) {.inline.} =
  tokenizer.varOrEnvFunctions = ssLooking

proc seenVarOrEnvFunctions*(tokenizer: Tokenizer): bool {.inline.} =
  let seen = tokenizer.varOrEnvFunctions == ssSeenAtleastOne
  tokenizer.varOrEnvFunctions = ssDontCare
  seen

proc seeFunction*(tokenizer: Tokenizer, name: string) {.inline.} =
  if tokenizer.varOrEnvFunctions == ssLooking:
    let lower = toLowerAscii(name)

    if lower == "env" or lower == "var":
      tokenizer.varOrEnvFunctions = ssSeenAtleastOne

proc hasAtLeast*(tokenizer: Tokenizer, n: uint): bool {.inline, gcsafe, noSideEffect.} =
  tokenizer.pos + n < tokenizer.input.len.uint

proc isEof*(tokenizer: Tokenizer): bool {.inline, gcsafe, noSideEffect.} =
  not tokenizer.hasAtLeast(0)

proc startsWith*(tokenizer: Tokenizer, needle: string): bool {.inline.} =
  tokenizer.input[tokenizer.pos..tokenizer.input.len-1].startsWith(needle)

proc slice*(tokenizer: Tokenizer, start, stop: uint): string {.inline.} =
  tokenizer.input[start..stop]

proc charAt*(tokenizer: Tokenizer, offset: uint = 0'u): char {.inline, noSideEffect.} =
  tokenizer.input[tokenizer.pos + offset]

proc nextChar*(tokenizer: Tokenizer): char {.inline, noSideEffect.} =
  charAt(tokenizer, 0)

proc forwards*(tokenizer: Tokenizer, n: uint) {.inline, gcsafe.} =
  tokenizer.pos += n

proc charToDecimalDigit*(c: char): Option[uint32] {.inline.} =
  if c >= '0' and c <= '9':
    return some((c.ord - '0'.ord).uint32)

proc consumeNewline*(tokenizer: Tokenizer) {.inline.} =
  let c = tokenizer.nextChar()
  assert c == '\r' or c == '\n' or c == '\x0C'

  inc tokenizer.pos
  if c == '\r' and tokenizer.nextChar() == '\n':
    inc tokenizer.pos

  tokenizer.currLineStartPos = tokenizer.pos
  inc tokenizer.currLineNumber

proc sliceFrom*(
  tokenizer: Tokenizer,
  start: uint
): string {.inline, noSideEffect.} =
  if start == tokenizer.pos:
    var thing = newString(1)
    thing[0] = tokenizer.input[start]

    return thing

  tokenizer.input[start..tokenizer.pos-1]

proc hasNewlineAt*(
  tokenizer: Tokenizer,
  offset: uint
): bool {.inline, gcsafe, noSideEffect.} =
  tokenizer.pos + offset < tokenizer.input.len.uint and
  tokenizer.charAt(offset) in ['\n', '\r', '\x0C']

proc consumeWhitespace*(tokenizer: Tokenizer, newline: bool): Token {.discardable.} =
  let startPos = tokenizer.pos
  if newline:
    tokenizer.consumeNewline()
  else:
    tokenizer.forwards(1)

  while not tokenizer.isEof():
    let c = tokenizer.nextChar()
    case c
    of ' ', '\t':
      tokenizer.forwards(1)
    of '\n', '\x0C', '\r':
      tokenizer.consumeNewline()
    else:
      break

  Token(
    kind: tkWhitespace,
    wsStr: tokenizer.sliceFrom(startPos)
  )

proc isIdentStart*(tokenizer: Tokenizer): bool {.inline.} =
  if tokenizer.isEof():
    return false

  case tokenizer.nextChar()
  of {'a'..'z'}, {'A'..'Z'}, '_', '\0': return true
  of '-':
    if not tokenizer.hasAtLeast(1):
      return false

    case tokenizer.charAt(1)
    of {'a'..'z'}, {'A'..'Z'}, '-', '_', '\0':
      return true
    of '\\':
      return not tokenizer.hasNewlineAt(1)
    else:
      return not tokenizer.charAt(1).isAlphaAscii()
  of '\\':
    return not tokenizer.hasNewlineAt(1)
  else:
    return not tokenizer.charAt(1).isAlphaAscii()


proc checkForSourceMap*(
  tokenizer: Tokenizer,
  contents: string
) =
  let
    directive = "# sourceMappingURL="
    directiveOld = "@ sourceMappingURL="

  if contents.startsWith(directive) or contents.startsWith(directiveOld):
    let contents = contents[directive.len..contents.len]
    tokenizer.sourceMapUrl = contents.some() # FIXME: this is not compliant!
  
  let
    directiveB = "# sourceURL="
    directiveBOld = "@ sourceURL="

  if contents.startsWith(directiveB) or contents.startsWith(directiveBOld):
    let contents = contents[directiveB.len..contents.len]
    tokenizer.sourceUrl = contents.some() # FIXME: this is not compliant!

proc consumeContinuationByte*(
  tokenizer: Tokenizer
) {.inline.} =
  inc tokenizer.currLineStartPos
  inc tokenizer.pos

proc consume4byteIntro*(
  tokenizer: Tokenizer
) {.inline.} =
  dec tokenizer.currLineStartPos
  inc tokenizer.pos

proc consumeEscape*(
  tokenizer: Tokenizer
): char =
  # TODO: implement this thing
  ' '

proc consumeEscapeAndWrite*(
  tokenizer: Tokenizer,
  str: var string
) {.inline.} =
  str &= tokenizer.consumeEscape()

proc consumeName*(tokenizer: Tokenizer): string =
  let start = tokenizer.pos
  var value: string
  
  while true:
    if tokenizer.isEof():
      return tokenizer.sliceFrom(start)

    case tokenizer.nextChar()
    of {'a'..'z'}, {'A'..'Z'}, {'0'..'9'}, '_', '-':
      tokenizer.forwards(1)
    of '\\', '\0':
      value = tokenizer.sliceFrom(start)
    of {'\x80'..'\xBF'}: tokenizer.consumeContinuationByte()
    of {'\xC0'..'\xEF'}: tokenizer.forwards(1)
    of {'\xF0'..'\xFF'}: tokenizer.consume4byteIntro()
    else:
      return tokenizer.sliceFrom(start)

  while not tokenizer.isEof():
    let c = tokenizer.nextChar()
    case c
    of {'a'..'z'}, {'A'..'Z'}, {'0'..'9'}, '_', '-':
      tokenizer.forwards(1)
      value &= c
    of '\\':
      if tokenizer.hasNewlineAt(1): break
      tokenizer.forwards(1)

      tokenizer.consumeEscapeAndWrite(value)
    of '\0':
      tokenizer.forwards(1)
      value &= "\u{FFFD}"
    of {'\x80'..'\xBF'}:
      tokenizer.consumeContinuationByte()
      value &= c
    of {'\xC0'..'\xEF'}:
      tokenizer.forwards(1)
      value &= c
    of {'\xF0'..'\xFF'}:
      tokenizer.consume4byteIntro()
      value &= c
    else:
      break
  
  value

proc consumeNumeric*(
  tokenizer: Tokenizer
): Token =
  let (hasSign, sign) = case tokenizer.nextChar():
    of '-':
      (true, -1f)
    of '+':
      (true, 1f)
    else:
      (false, 1f)

  if hasSign:
    tokenizer.forwards(1)

  var 
    integralPart: float64
    digit: uint32

  while unpack(
    charToDecimalDigit(tokenizer.nextChar()),
    digit
  ):
    integralPart = integralPart * 10'f64 + digit.float64
    tokenizer.forwards(1)
    if tokenizer.isEof():
      break

  var 
    isInteger = true
    fractionalPart: float64 = 0'f64

  if tokenizer.hasAtleast(1) and tokenizer.nextChar() == '.' and tokenizer.charAt(1) in {'0'..'9'}:
    isInteger = false
    tokenizer.forwards(1)

    var factor = 0.1'f64

    while unpack(
      charToDecimalDigit(tokenizer.nextChar()),
      digit
    ):
      fractionalPart += digit.float64 * factor
      factor *= 0.1'f64
      tokenizer.forwards(1)
      if tokenizer.isEof():
        break
  
  var value = sign * (integralPart + fractionalPart)
  if tokenizer.hasAtleast(1) and tokenizer.nextChar() in ['e', 'E']:
    if tokenizer.charAt(1) in {'0'..'9'} or
      tokenizer.hasAtleast(2) and
        tokenizer.charAt(1) in ['+', '-'] and
        tokenizer.charAt(2) in {'0'..'9'}:
          isInteger = false
          tokenizer.forwards(1)

          let (hasSign, sign) = case tokenizer.nextChar()
          of '-':
            (true, -1f)
          of '+':
            (true, 1f)
          else:
            (false, 1f)

          if hasSign:
            tokenizer.forwards(1)

          var exponent: float64 = 0'f64

          while unpack(
            charToDecimalDigit(tokenizer.nextChar()),
            digit
          ):
            exponent = exponent * 10'f64 + digit.float64
            tokenizer.forwards(1)
            if tokenizer.isEof():
              break

          value *= pow(10'f64, sign * exponent)

  var intValue: Option[int32]

  if isInteger: 
    intValue = some(
      if value >= int32.high.float64: int32.high
      elif value <= int32.low.float64: int32.low
      else:
        value.int32
    )
  
  if not tokenizer.isEof() and tokenizer.nextChar() == '%':
    tokenizer.forwards(1)
    return Token(
      kind: tkPercentage,
      pHasSign: hasSign,
      pIntVal: intValue,
      pUnitValue: (value / 100'f64).float32
    )

  let valF32 = value.float32

  if tokenizer.isIdentStart():
    let unit = tokenizer.consumeName()
    return Token(
      kind: tkDimension,
      dHasSign: hasSign,
      dValue: valF32,
      dIntVal: intValue,
      unit: unit
    )
  else:
    return Token(
      kind: tkNumber,
      nHasSign: hasSign,
      nValue: valF32,
      nIntVal: intValue
    )

proc consumeQuotedString*(
  tokenizer: Tokenizer,
  singleQuote: bool
): QuotedString =
  tokenizer.forwards(1)

  let start = tokenizer.pos
  var str: string

  while true:
    if tokenizer.isEof():
      return ok tokenizer.sliceFrom(start)

    case tokenizer.nextChar()
    of '"':
      if not singleQuote:
        let value = tokenizer.sliceFrom(start)
        tokenizer.forwards(1)
        return ok(value)

      tokenizer.forwards(1)
    of '\'':
      if singleQuote:
        let value = tokenizer.sliceFrom(start)
        tokenizer.forwards(1)
        return ok(value)
    of '\\', '\0':
      str = tokenizer.sliceFrom(start)
      break
    of '\n', '\r', '\x0C':
      return err(
        tokenizer.sliceFrom(start)
      )
    of {'\x80'..'\xBF'}:
      tokenizer.consumeContinuationByte()
    of {'\xF0'..'\xFF'}:
      tokenizer.consume4byteIntro()
    else:
      tokenizer.forwards(1)

  while not tokenizer.isEof():
    let c = tokenizer.nextChar()
    case c
    of '\n', '\r', '\x0C':
      return err(
        str
      )
    of '"':
      tokenizer.forwards(1)
      if not singleQuote:
        break
    of '\'':
      tokenizer.forwards(1)
      if singleQuote:
        break
    of '\\':
      tokenizer.forwards(1)
      if not tokenizer.isEof():
        case tokenizer.nextChar()
        of '\n', '\x0C', '\r':
          tokenizer.consumeNewline()
        else: tokenizer.consumeEscapeAndWrite(str)

      continue
    of '\0':
      tokenizer.forwards(1)
      str &= ' ' # FIXME: not compliant.
    of {'\x80'..'\xBF'}: tokenizer.consumeContinuationByte()
    of {'\xF0'..'\xFF'}: tokenizer.consume4byteIntro()
    else:
      tokenizer.forwards(1)

    str &= c

  ok(
    str
  )

proc consumeString*(
  tokenizer: Tokenizer,
  singleQuote: bool
): Token =
  let res = tokenizer.consumeQuotedString(singleQuote)

  if res.isOk:
    Token(
      kind: tkQuotedString,
      qStr: res.get()
    )
  else:
    Token(
      kind: tkBadString,
      badString: res.error()
    )

proc consumeComment*(
  tokenizer: Tokenizer
): string =
  tokenizer.forwards(2)

  let start = tokenizer.pos

  while not tokenizer.isEof():
    case tokenizer.nextChar()
    of '*':
      let endPos = tokenizer.pos-1
      tokenizer.forwards(1)
      if tokenizer.nextChar() == '/':
        tokenizer.forwards(1)
        let contents = tokenizer.slice(start, endPos)
        tokenizer.checkForSourceMap(contents)
        return contents
    of '\n', '\x0C', '\r':
      tokenizer.consumeNewline()
    of {'\x80'..'\xBF'}: tokenizer.consumeContinuationByte()
    of {'\xF0'..'\xFF'}: tokenizer.consume4byteIntro()
    else:
      tokenizer.forwards(1)

  let contents = tokenizer.sliceFrom(start)
  tokenizer.checkForSourceMap(contents)
  contents

proc seeFunction*(tokenizer: Tokenizer, value: var string) =
  if tokenizer.varOrEnvFunctions == ssLooking:
    if value.toLowerAscii() in ["var", "env"]:
      tokenizer.varOrEnvFunctions = ssSeenAtleastOne

proc consumeIdentLike*(tokenizer: Tokenizer): Token =
  let value = tokenizer.consumeName()

  if not tokenizer.isEof() and tokenizer.nextChar() == '(':
    tokenizer.seeFunction(value)
    return Token(kind: tkFunction, fnName: value)
  else:
    return Token(kind: tkIdent, ident: value)

proc consumeUnquotedUrl*(tokenizer: Tokenizer): Token =
  let 
    start = tokenizer.pos
    fromStart = tokenizer.input[tokenizer.pos-1]

  var
    newlines = 0
    lastNewline = 0
    foundPrintableChar = false

    iidx = -1
    iter = fromStart

  while true:
    inc iidx
    # TODO(xTrayambak): implement this function once you're done with the exams!

proc nextToken*(tokenizer: Tokenizer): Token =
  if tokenizer.isEof():
    raise newException(TokenizerDefect, "EOF reached!")

  let c = tokenizer.nextChar()
  var token: Token
  case c:
    of ' ', '\t':
      token = tokenizer.consumeWhitespace(false)
    of '\n', '\x0C', '\r': token = tokenizer.consumeWhitespace(true)
    of '"': token = tokenizer.consumeString(false)
    of '#':
      tokenizer.forwards(1)
      if isIdentStart(tokenizer):
        token = Token(kind: tkIDHash, idHash: tokenizer.consumeName())
      elif not tokenizer.isEof():
        case tokenizer.nextChar()
        of {'0'..'9'}, '-':
          token = Token(kind: tkHash, hash: tokenizer.consumeName())
        else: discard
    of '$':
      if tokenizer.startsWith("$="):
        tokenizer.forwards(2)
        token = Token(kind: tkSuffixMatch)
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '$')
    of '\'':
      token = tokenizer.consumeString(true)
    of '(':
      tokenizer.forwards(1)
      token = Token(kind: tkParenBlock)
    of ')':
      tokenizer.forwards(1)
      token = Token(kind: tkCloseParen)
    of '*':
      if tokenizer.startsWith("*="):
        tokenizer.forwards(2)
        token = Token(kind: tkSubstringMatch)
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '*')
    of '+':
      if tokenizer.hasAtleast(1) and tokenizer.charAt(1) in {'0'..'9'} or
        tokenizer.hasAtleast(2) and tokenizer.charAt(1) == '.' and tokenizer.charAt(2) in {'0'..'9'}:
          token = tokenizer.consumeNumeric()
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '+')
    of '-':
      if tokenizer.hasAtleast(1) and tokenizer.charAt(1) in {'0'..'9'} or
        tokenizer.hasAtleast(2) and tokenizer.charAt(1) == '.' and tokenizer.charAt(2) in {'0'..'9'}:
          token = tokenizer.consumeNumeric()
      elif tokenizer.startsWith("-->"):
        tokenizer.forwards(3)
        token = Token(kind: tkCDC)
      elif tokenizer.isIdentStart():
        token = tokenizer.consumeIdentLike()
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '-')
    of ',':
      tokenizer.forwards(1)
      token = Token(kind: tkComma)
    of '.':
      if tokenizer.hasAtleast(1) and tokenizer.charAt(1) in {'0'..'9'}:
        token = tokenizer.consumeNumeric()
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '.')
    of '/':
      if tokenizer.startsWith("/*"):
        token = Token(kind: tkComment, comment: tokenizer.consumeComment())
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '/')
    of {'0'..'9'}:
      token = tokenizer.consumeNumeric()
    of ':':
      tokenizer.forwards(1)
      token = Token(kind: tkColon)
    of ';':
      tokenizer.forwards(1)
      token = Token(kind: tkSemicolon)
    of '<':
      if tokenizer.startsWith("<!--"):
        tokenizer.forwards(4)
        token = Token(kind: tkCDO)
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '<')
    of '@':
      tokenizer.forwards(1)
      if isIdentStart(tokenizer):
        token = Token(kind: tkAtKeyword, at: tokenizer.consumeName())
      else:
        token = Token(kind: tkDelim, delim: '@')
    of {'a'..'z'}, {'A'..'Z'}, '_', '\0':
      token = tokenizer.consumeIdentLike()
    of '[':
      token = Token(kind: tkSquareBracketBlock)
    of '\\':
      tokenizer.forwards(1)
      if not tokenizer.hasNewlineAt(1):
        token = tokenizer.consumeIdentLike()
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '\\')
    of ']':
      tokenizer.forwards(1)
      token = Token(kind: tkCloseSquareBracket)
    of '^':
      if tokenizer.startsWith("^="):
        tokenizer.forwards(2)
        token = Token(kind: tkPrefixMatch)
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '^')
    of '{':
      tokenizer.forwards(1)
      token = Token(kind: tkCurlyBracketBlock)
    of '|':
      if tokenizer.startsWith("|="):
        tokenizer.forwards(2)
        token = Token(kind: tkDashMatch)
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '|')
    of '}':
      tokenizer.forwards(1)
      token = Token(kind: tkCloseCurlyBracket)
    of '~':
      if tokenizer.startsWith("~="):
        tokenizer.forwards(1)
        token = Token(kind: tkIncludeMatch)
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: '~')
    else:
      if not c.isAlphaAscii():
        token = tokenizer.consumeIdentLike()
      else:
        tokenizer.forwards(1)
        token = Token(kind: tkDelim, delim: c)

  token
