import std/[sugar, options], results

import ./[shared, tokenizer, utils]

type
  ParseUntilErrorBehaviour* = enum
    peConsume
    peStop

  BasicParseErrorKind* = enum
    bpUnexpectedToken
    bpEndOfInput
    bpAtRuleInvalid
    bpAtRuleBodyInvalid
    bpQualifiedRuleInvalid

  BasicParseError* = ref object
    case kind*: BasicParseErrorKind
    of bpUnexpectedToken:
      token*: Token
    of bpAtRuleInvalid:
      rule*: string
    else:
      discard

    location*: SourceLocation

  CachedToken* = ref object
    token*: Token
    startPos*: SourcePosition
    endState*: ParserState

  ParserInput* = ref object
    tokenizer*: Tokenizer
    cachedToken*: Option[CachedToken]

  Delimiters* = object
    bits*: byte

  Parser* = ref object
    input*: ParserInput
    atStartOf*: Option[BlockType]
    stopBefore*: Delimiters

const
  DelimNone* = Delimiters(
    bits: 0'u8
  )
  DelimCurlyBracketBlock* = Delimiters(
    bits: 1 shl 1
  )
  DelimSemicolon* = Delimiters(
    bits: 1 shl 2
  )
  DelimBang* = Delimiters(
    bits: 1 shl 3
  )
  DelimComma* = Delimiters(
    bits: 1 shl 4
  )
  CloseCurlyBracket* = Delimiters(
    bits: 1 shl 5
  )
  CloseSquareBracket* = Delimiters(
    bits: 1 shl 6
  )
  CloseParenthesis* = Delimiters(
    bits: 1 shl 7
  )

  uSemi = uint ';'
  uBang = uint '!'
  uComma = uint ','
  uCBB = uint '{'
  uCCB = uint '}'
  uCSB = uint ']'
  uCP = uint ')'

# welp, we can't make it an array, I guess
const TABLE*: seq[Delimiters] = collect(newSeqOfCap(256)):
  for x in 0..256:
    let ux = uint x
    if ux notin [uSemi, uBang, uComma, uCBB, uCCB, uCSB, uCP]: DelimNone
    else:
      var res: Delimiters
      case ux
      of uSemi: res = DelimSemicolon
      of uBang: res = DelimBang
      of uComma: res = DelimComma
      of uCBB: res = DelimCurlyBracketBlock
      of uCCB: res = CloseCurlyBracket
      of uCSB: res = CloseSquareBracket
      of uCP: res = CloseParenthesis
      else: res = DelimNone # this can never happen

      res

proc newParserInput*(
  input: string
): ParserInput {.inline.} =
  ParserInput(
    tokenizer: newTokenizer(input),
    cachedToken: none(CachedToken)
  )

proc newParser*(
  input: ParserInput
): Parser {.inline.} =
  Parser(
    input: input,
    atStartOf: none(BlockType),
    stopBefore: DelimNone
  )

proc contains*(self, other: Delimiters): bool {.inline.} =
  (self.bits and other.bits) != 0

proc fromChar*(c: Option[char]): Delimiters {.inline.} =
  if c.isNone:
    return DelimNone

  TABLE[uint8 c.unsafeGet()]

proc currLine*(parser: Parser): string {.inline.} =
  parser.input.tokenizer.currentSourceLine()

proc currSourceLocation*(
  parser: Parser
): SourceLocation {.inline.} =
  parser.input.tokenizer.currSourceLocation()

proc newBasicError*(
  parser: Parser,
  kind: BasicParseErrorKind
): BasicParseError {.inline.} =
  BasicParseError(
    kind: kind,
    location: parser.currSourceLocation()
  )

proc reset*(parser: Parser, state: ParserState) {.inline.} =
  parser.input.tokenizer.reset(state)
  parser.atStartOf = state.atStartOf

proc opening*(token: Token): Option[BlockType] {.inline, noSideEffect.} =
  result = case token.kind
  of tkFunction, tkParenBlock:
    some(btParenthesis)
  of tkSquareBracketBlock:
    some(btSquareBracket)
  of tkCurlyBracketBlock:
    some(btCurlyBracket)
  else: none(BlockType)

proc closing*(token: Token): Option[BlockType] {.inline, noSideEffect.} =
  result = case token.kind
  of tkCloseParen: 
    some(btParenthesis)
  of tkCloseSquareBracket: 
    some(btSquareBracket)
  of tkCloseCurlyBracket:
    some(btCurlyBracket)
  else:
    none(BlockType)

proc `$`*(error: BasicParseError): string =
  case error.kind
  of bpUnexpectedToken:
    return "unexpected token: "
  of bpEndOfInput:
    return "unexpected EOF"
  of bpAtRuleInvalid:
    return "invalid @ rule encountered: " & error.rule
  of bpAtRuleBodyInvalid:
    return "invalid @ rule body encountered"
  of bpQualifiedRuleInvalid:
    return "invalid qualified role encountered"

proc parseError*(
    kind: BasicParseErrorKind,
    token: Option[Token],
    rule: Option[string],
    location: SourceLocation,
): BasicParseError =
  var err = BasicParseError(kind: kind)

  case kind
  of bpUnexpectedToken:
    if not token.isSome:
      raise newException(
          ValueError, "bpUnexpectedToken passed, but `token` argument was left empty!"
        )

    err.token = get(token)
  of bpAtRuleInvalid:
    if not rule.isSome:
      raise newException(
          ValueError, "bpAtRuleInvalid passed, but `rule` argument was left empty!"
        )

    err.rule = get(rule)
  else:
    discard

  err

proc basicUnexpectedTokenError*(location: SourceLocation, token: Token): BasicParseError {.inline.} =
  BasicParseError(kind: bpUnexpectedToken, token: token, location: location)

proc position*(state: ParserState): SourcePosition {.inline.} =
  SourcePosition(state.position)

proc sourceLocation*(state: ParserState): SourceLocation {.inline.} =
  SourceLocation(
    line: state.currentLineNumber.uint32,
    column: (state.position - state.currentLineStartPos + 1'u32).uint32,
  )

proc consumeUntilEndOfBlock*(blockType: BlockType, tokenizer: Tokenizer) =
  var stack = newSeq[BlockType](16)
  stack.add(blockType)

  var ctk = tokenizer.nextToken()

  while ctk != nil:
    let closingBk = closing ctk

    if closingBk.isSome:
      if stack[-1] == closingBk.unsafeGet():
        discard pop stack
        if stack.len < 1:
          return

    let openingBk = opening ctk

    if openingBk.isSome:
      stack.add(openingBk.unsafeGet())

proc skipWhitespace*(parser: Parser) {.inline.} =
  if parser.atStartOf.isSome:
    consumeUntilEndOfBlock(parser.atStartOf.unsafeGet(), parser.input.tokenizer)

  parser.input.tokenizer.skipWhitespace()

proc skipCdcAndCdo*(parser: Parser) {.inline.} =
  if parser.atStartOf.isSome:
    consumeUntilEndOfBlock(parser.atStartOf.unsafeGet(), parser.input.tokenizer)

  parser.input.tokenizer.skipCdcAndCdo()

proc state*(parser: Parser): ParserState {.inline.} =
  ParserState(
    atStartOf: parser.atStartOf,
    position: parser.input.tokenizer.pos,
    currentLineStartPos: parser.input.tokenizer.currLineStartPos,
    currentLineNumber: parser.input.tokenizer.currLineNumber
  )

proc nextIncludingWhitespaceAndComments*(
  parser: Parser
): Result[Token, BasicParseError] =
  var blockType: BlockType
  if &parser.atStartOf:
    blockType = parser.atStartOf.get()
    consumeUntilEndOfBlock(blockType, parser.input.tokenizer)

  let c = parser.input.tokenizer.nextChar()
  if parser.stopBefore.contains(fromChar(some c)):
    return err(
      parser.newBasicError(
        bpEndOfInput
      )
    )

  let
    tokenStartPos = parser.input.tokenizer.position()
    usingCachedToken = if parser.input.cachedToken.isSome:
      parser.input.cachedToken.unsafeGet().startPos == tokenStartPos
    else:
      false

  var token: Token

  if usingCachedToken:
    let cachedToken = parser.input.cachedToken.unsafeGet() # we already verified that it isn't an empty option, so it's fine (hopefully)
    parser.input.tokenizer.reset(cachedToken.endState)
    case cachedToken.token.kind
    of tkFunction:
      parser.input.tokenizer.seeFunction(cachedToken.token.fnName)
    else: discard

    token = cachedToken.token
  else:
    let newToken = parser
      .input
      .tokenizer
      .nextToken()

    parser.input.cachedToken = some(
      CachedToken(
        token: newToken,
        startPos: tokenStartPos,
        endState: parser.input.tokenizer.state()
      )
    )

    token = newToken

  let cBlockType = closing token

  if cBlockType.isSome:
    parser.atStartOf = cBlockType

  ok(token)

proc next*(parser: Parser): Result[Token, BasicParseError] =
  parser.skipWhitespace()
  parser.nextIncludingWhitespaceAndComments()

proc expectExhausted*(
  parser: Parser
): Result[bool, BasicParseError] =
  let 
    start = parser.state
    next = parser.next()

    res = case next.isErr()
    of false:
      err(
        start
          .sourceLocation()
          .basicUnexpectedTokenError(next.get())
      )
    of true:
      err(
        BasicParseError(
          kind: bpEndOfInput
        )
      )

  parser.reset(start)
  res
