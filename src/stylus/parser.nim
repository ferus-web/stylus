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

  ParseErrorKind* = enum
    peBasic
    peCustom

  ParseError*[E] = ref object
    case kind*: ParseErrorKind
    of peBasic:
      basic*: E
    of peCustom:
      custom*: E

    location*: SourceLocation

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
    if not &token:
      raise newException(
          ValueError, "bpUnexpectedToken passed, but `token` argument was left empty!"
        )

    err.token = get(token)
  of bpAtRuleInvalid:
    if not &rule:
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

    if &closingBk:
      if stack[-1] == closingBk.unsafeGet():
        discard pop stack
        if stack.len < 1:
          return

    let openingBk = opening ctk

    if &openingBk:
      stack.add(openingBk.unsafeGet())

proc skipWhitespace*(parser: Parser) {.inline.} =
  if &parser.atStartOf:
    consumeUntilEndOfBlock(parser.atStartOf.unsafeGet(), parser.input.tokenizer)

  parser.input.tokenizer.skipWhitespace()

proc newBasicUnexpectedTokenError*(
  location: SourceLocation, 
  token: Token
): BasicParseError {.inline.} =
  BasicParseError(
    kind: bpUnexpectedToken,
    token: token
  )

proc skipCdcAndCdo*(parser: Parser) {.inline.} =
  if &parser.atStartOf:
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

  if &cBlockType:
    parser.atStartOf = cBlockType

  ok(token)

proc next*(parser: Parser): Result[Token, BasicParseError] =
  parser.skipWhitespace()
  parser.nextIncludingWhitespaceAndComments()

template expect*(
  parser: Parser,
  body: untyped
) =
  # does this actually work like it's supposed to?
  let
    start = parser.currSourceLocation()
    next = parser.next()

  if next.isOk:
    let value {.inject.} = next.get()

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

proc position*(
  parser: Parser
): SourcePosition {.inline.} =
  parser.input.tokenizer.position()

proc newError*(parser: Parser, err: BasicParseError): ParseError {.inline.} =
  ParseError[BasicParseError] (
    kind: peBasic,
    basic: err,
    location: parser.currSourceLocation()
  )

proc newErrorForNextToken*(parser: Parser): ParseError {.inline.} =
  let t = parser.next()
  
  let token = if t.isOk:
    deepCopy(t)

  let err = if t.isErr:
    t.error()

  parser.newError(
    BasicParseError(
      kind: bpUnexpectedToken,
      token: token
    )
  )

proc nextChar*(parser: Parser): Option[char] {.inline.} =
  let c = some parser.input.tokenizer.nextChar()

  if fromChar(c) in parser.stopBefore:
    return

  c

proc lookForVarOrEnvFunctions*(parser: Parser) {.inline.} =
  parser.input.tokenizer.lookForVarOrEnvFunctions()

proc seenVarOrEnvFunctions*(parser: Parser): bool {.inline.} =
  parser.seenVarOrEnvFunctions()

# generics hell 2: electric boogaloo
proc tryParse*[T, E](
  parser: Parser,
  thing: proc(parser: Parser): Result[T, E]
): Result[T, E] =
  let 
    start = parser.state()
    res = thing(parser)

  if res.isErr:
    parser.reset(start)

  res

proc slice*(
  parser: Parser,
  range: Slice[SourcePosition]
): string {.inline.} =
  parser.input.tokenizer.slice(range)

proc sliceFrom*(
  parser: Parser,
  start: SourcePosition
): string {.inline.} =
  parser.input.tokenizer.sliceFrom(start)

proc parseEntirely*[T, E](
  parser: Parser,
  parse: proc(parser: Parser): Result[T, ParseError[E]]
) {.inline.} =
  let res = parse parser
  parser.expectExhausted()
  ok res

proc parseUntilBefore*[T, E](
  parser: Parser,
  delimiters: Delimiters,
  errorBehaviour: ParseUntilErrorBehaviour,
  parse: proc(parser: Parser): Result[T, ParseError[E]]
) =
  let delimiters = parser.stopBefore or delimiters

  var delimitedParser = Parser(
    input: parser.input,
    atStartOf: parser.atStartOf,
    stopBefore: delimiters
  )
  let res = delimitedParser.parseEntirely(parse)
  if errorBehaviour == peStop and res.isErr:
    return res

  let blockType = delimitedParser.atStartOf

  if &blockType:
    consumeUntilEndOfBlock(blockType, delimitedParser.input.tokenizer)

  while true:
    if parser.input.tokenizer.nextChar().some().fromChar() in delimiters:
      break

    let token = parser.input.tokenizer.nextToken()

    if token.isOk:
      let blckType = opening token.get()
      if &blckType:
        consumeUntilEndOfBlock(blckType, parser.input.tokenizer)
    else:
      break

  res

proc parseUntilBefore*[T, E](
  parser: Parser,
  delimiters: Delimiters,
  parse: proc(parser: Parser): Result[T, ParseError[E]]
) {.inline.} =
  parseUntilBefore(parser, delimiters, peConsume, parse)

proc parseUntilAfter*[T, E](
  parser: Parser,
  delimiters: Delimiters,
  errorBehaviour: ParseUntilErrorBehaviour,
  parse: proc(parser: Parser): Result[T, ParseError[E]]
) {.inline.} =
  let res = parseUntilBefore(parser, delimiters, errorBehaviour, parse)
  if errorBehaviour == peStop and res.isErr:
    return res

  let c = parser.input.tokenizer.nextChar()
  if c.some().fromChar() notin parser.stopBefore:
    parser.input.tokenizer.forwards(1)
    if c == '{':
      consumeUntilEndOfBlock(btCurlyBracket, parser.input.tokenizer)

  res

proc parseUntilAfter*[T, E](
  parser: Parser,
  delimiters: Delimiters,
  parse: proc(parser: Parser): Result[T, ParseError[E]]
) {.inline.} =
  parseUntilAfter(parser, delimiters, peConsume, parse)

proc nextIncludingWhitespace*(parser: Parser): Result[Token, BasicParseError] =
  while true:
    let res = parser.nextIncludingWhitespaceAndComments()

    if res.isErr:
      return res.error().err()

    if res.isOk:
      continue

    break

  ok(parser.input.cachedToken.get().token)

proc expectWhitespace*(parser: Parser): Result[string, BasicParseError] {.inline.} =
  let 
    start = parser.currSourceLocation()
    next = parser.nextIncludingWhitespace().get()

  case next.kind
  of tkWhitespace:
    return ok(next.wsStr)
  else:
    err(
      start.newBasicUnexpectedTokenError(deepCopy next)
    )
