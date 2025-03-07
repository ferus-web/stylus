import std/[sugar, options, strutils, sequtils], results

import ./[shared, tokenizer, utils]
import pretty

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

  ParserDefect* = object of Defect
    ## Unrecoverable errors in the parser's logic which are meant to never fail under normal circumstances

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
  DelimNone* = Delimiters(bits: 0'u8)
  DelimCurlyBracketBlock* = Delimiters(bits: 1 shl 1)
  DelimSemicolon* = Delimiters(bits: 1 shl 2)
  DelimBang* = Delimiters(bits: 1 shl 3)
  DelimComma* = Delimiters(bits: 1 shl 4)
  CloseCurlyBracket* = Delimiters(bits: 1 shl 5)
  CloseSquareBracket* = Delimiters(bits: 1 shl 6)
  CloseParenthesis* = Delimiters(bits: 1 shl 7)

  uSemi = uint ';'
  uBang = uint '!'
  uComma = uint ','
  uCBB = uint '{'
  uCCB = uint '}'
  uCSB = uint ']'
  uCP = uint ')'

# TODO: welp, we can't make it an array, I guess
const TABLE*: seq[Delimiters] = collect(newSeqOfCap(256)):
  for x in 0 .. 256:
    let ux = uint x
    if ux notin [uSemi, uBang, uComma, uCBB, uCCB, uCSB, uCP]:
      DelimNone
    else:
      var res: Delimiters
      case ux
      of uSemi:
        res = DelimSemicolon
      of uBang:
        res = DelimBang
      of uComma:
        res = DelimComma
      of uCBB:
        res = DelimCurlyBracketBlock
      of uCCB:
        res = CloseCurlyBracket
      of uCSB:
        res = CloseSquareBracket
      of uCP:
        res = CloseParenthesis
      else:
        res = DelimNone
        # this can never happen

      res

proc newParserInput*(input: string): ParserInput {.inline.} =
  ParserInput(tokenizer: newTokenizer(input), cachedToken: none(CachedToken))

proc newParser*(input: ParserInput): Parser {.inline.} =
  Parser(input: input, atStartOf: none(BlockType), stopBefore: DelimNone)

proc contains*(self, other: Delimiters): bool {.inline.} =
  (self.bits and other.bits) != 0

proc fromChar*(c: Option[char]): Delimiters {.inline.} =
  if c.isNone:
    return DelimNone

  TABLE[uint8 c.unsafeGet()]

proc currLine*(parser: Parser): string {.inline.} =
  parser.input.tokenizer.currentSourceLine()

proc currSourceLocation*(parser: Parser): SourceLocation {.inline.} =
  parser.input.tokenizer.currSourceLocation()

proc newBasicError*(
    parser: Parser, kind: BasicParseErrorKind
): BasicParseError {.inline.} =
  BasicParseError(kind: kind, location: parser.currSourceLocation())

proc reset*(parser: Parser, state: ParserState) {.inline.} =
  parser.input.tokenizer.reset(state)
  parser.atStartOf = state.atStartOf

proc opening*(token: Token): Option[BlockType] {.inline, noSideEffect.} =
  result =
    case token.kind
    of tkFunction, tkParenBlock:
      some(btParenthesis)
    of tkSquareBracketBlock:
      some(btSquareBracket)
    of tkCurlyBracketBlock:
      some(btCurlyBracket)
    else:
      none(BlockType)

proc closing*(token: Token): Option[BlockType] {.inline, noSideEffect.} =
  result =
    case token.kind
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

proc basicUnexpectedTokenError*(
    location: SourceLocation, token: Token
): BasicParseError {.inline.} =
  BasicParseError(kind: bpUnexpectedToken, token: token, location: location)

proc position*(state: ParserState): SourcePosition {.inline.} =
  SourcePosition(state.position)

proc sourceLocation*(state: ParserState): SourceLocation {.inline.} =
  SourceLocation(
    line: state.currentLineNumber.uint32,
    column: (state.position - state.currentLineStartPos + 1'u32).uint32,
  )

proc consumeUntilEndOfBlock*(blockType: BlockType, tokenizer: Tokenizer) =
  var stack: seq[BlockType]
  stack &= blockType

  while not tokenizer.isEof:
    let ctk = tokenizer.nextToken()
    let closingBk = closing ctk

    if &closingBk:
      if stack[stack.len - 1] == closingBk.unsafeGet():
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
    location: SourceLocation, token: Token
): BasicParseError {.inline.} =
  BasicParseError(kind: bpUnexpectedToken, token: token)

proc newBasicUnexpectedTokenError*(
    parser: Parser, token: Token
): BasicParseError {.inline.} =
  newBasicUnexpectedTokenError(parser.currSourceLocation(), token)

proc skipCdcAndCdo*(parser: Parser) {.inline.} =
  if &parser.atStartOf:
    consumeUntilEndOfBlock(parser.atStartOf.unsafeGet(), parser.input.tokenizer)

  parser.input.tokenizer.skipCdcAndCdo()

proc state*(parser: Parser): ParserState {.inline.} =
  ParserState(
    atStartOf: parser.atStartOf,
    position: parser.input.tokenizer.pos,
    currentLineStartPos: parser.input.tokenizer.currLineStartPos,
    currentLineNumber: parser.input.tokenizer.currLineNumber,
  )

proc nextIncludingWhitespaceAndComments*(
    parser: Parser
): Result[Token, BasicParseError] =
  if parser.input.tokenizer.isEof:
    return err(parser.newBasicError(bpEndOfInput))

  if &parser.atStartOf:
    let blockType = parser.atStartOf.get()
    consumeUntilEndOfBlock(blockType, parser.input.tokenizer)

  let c = parser.input.tokenizer.nextChar()
  if parser.stopBefore.contains(fromChar(some c)):
    return err(parser.newBasicError(bpEndOfInput))

  let token = parser.input.tokenizer.nextToken()

  #[ if &cBlockType:
    parser.atStartOf = cBlockType ]#

  ok(token)

proc next*(parser: Parser): Result[Token, BasicParseError] =
  parser.skipWhitespace()
  parser.nextIncludingWhitespaceAndComments()

proc expect*[T](
    parser: Parser,
    wants: openArray[TokenKind],
    fn: proc(token: Token): Result[T, BasicParseError],
): Result[T, BasicParseError] {.inline.} =
  let
    start = parser.currSourceLocation()
    next = parser.next()

  if next.isOk:
    let value {.inject.} = next.get()
    if value.kind notin wants:
      return err(start.newBasicUnexpectedTokenError(deepCopy value))
    else:
      return fn(value)
  else:
    return err(next.error())

proc expectExhausted*(parser: Parser): Result[bool, BasicParseError] =
  let
    start = parser.state
    next = parser.next()

    res =
      case next.isErr()
      of false:
        err(start.sourceLocation().basicUnexpectedTokenError(next.get()))
      of true:
        err(BasicParseError(kind: bpEndOfInput))

  parser.reset(start)
  res

proc isExhausted*(parser: Parser): bool {.inline.} =
  parser
    .expectExhausted()
    .isOk()

proc position*(parser: Parser): SourcePosition {.inline.} =
  parser.input.tokenizer.position()

proc newError*(parser: Parser, err: BasicParseError): ParseError {.inline.} =
  ParseError[BasicParseError] (
    kind: peBasic, basic: err, location: parser.currSourceLocation()
  )

proc newErrorForNextToken*(parser: Parser): ParseError {.inline.} =
  let t = parser.next()

  let token =
    if t.isOk:
      deepCopy(t)

  let err =
    if t.isErr:
      t.error()

  parser.newError(BasicParseError(kind: bpUnexpectedToken, token: token))

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
    parser: Parser, thing: proc(parser: Parser): Result[T, E]
): Result[T, E] =
  let
    start = parser.state()
    res = thing(parser)

  if res.isErr:
    parser.reset(start)

  res

proc slice*(parser: Parser, range: Slice[SourcePosition]): string {.inline.} =
  parser.input.tokenizer.slice(range)

proc sliceFrom*(parser: Parser, start: SourcePosition): string {.inline.} =
  parser.input.tokenizer.sliceFrom(start)

proc parseEntirely*[T, E](
    parser: Parser, parse: proc(parser: Parser): Result[T, ParseError[E]]
): Result[T, ParseError[E]] {.inline.} =
  let res = parse parser
  discard parser.expectExhausted()
  res

proc parseUntilBefore*[T, E](
    parser: Parser,
    delimiters: Delimiters,
    errorBehaviour: ParseUntilErrorBehaviour,
    parse: proc(parser: Parser): Result[T, ParseError[E]],
) =
  let delimiters = parser.stopBefore or delimiters

  var delimitedParser =
    Parser(input: parser.input, atStartOf: parser.atStartOf, stopBefore: delimiters)
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
    parse: proc(parser: Parser): Result[T, ParseError[E]],
) {.inline.} =
  parseUntilBefore(parser, delimiters, peConsume, parse)

proc parseUntilAfter*[T, E](
    parser: Parser,
    delimiters: Delimiters,
    errorBehaviour: ParseUntilErrorBehaviour,
    parse: proc(parser: Parser): Result[T, ParseError[E]],
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
    parse: proc(parser: Parser): Result[T, ParseError[E]],
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
    err(start.newBasicUnexpectedTokenError(deepCopy next))

proc parseNestedBlock*[T, E](
    parser: Parser, parse: proc(parser: Parser): Result[T, E]
): Result[T, E] =
  if parser.atStartOf.isNone:
    raise newException(
      ParserDefect,
      "A nested parser can only be created when a tkFunction, tkParenBlock, tkSquareBracketBlock or tkCurlyBracketBlock " &
        "token was just consumed.",
    )

  let
    blockType = parser.atStartOf.unsafeGet()
    closingDelim =
      case blockType
      of btCurlyBracket: CloseCurlyBracket
      of btSquareBracket: CloseSquareBracket
      of btParenthesis: CloseParenthesis

  var nestedParser =
    Parser(input: parser.input, atStartOf: none(BlockType), stopBefore: closingDelim)

  let res = nestedParser.parseEntirely(parse)

  let blkType = nestedParser.atStartOf

  if &blkType:
    consumeUntilEndOfBlock(blkType.unsafeGet(), nestedParser.input.tokenizer)

  consumeUntilEndOfBlock(blockType, nestedParser.input.tokenizer)
  res

proc expectIdent*(parser: Parser): Result[string, BasicParseError] {.inline.} =
  # FIXME: this is incredibly dumb!
  proc inner(
      token: Token
  ): Result[string, BasicParseError] {.inline, gcsafe, noSideEffect.} =
    ok(token.ident)

  expect parser, [tkIdent], (token: Token) => inner token

proc expectIdentMatching*(
    parser: Parser, expectedValue: string
): Result[string, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[string, BasicParseError] {.gcsafe, noSideEffect.} =
    if token.ident.toLowerAscii() == expectedValue:
      ok token.ident
    else:
      err parser.newBasicUnexpectedTokenError(token)

  expect parser, [tkIdent], (token: Token) => inner token

proc expectString*(parser: Parser): Result[string, BasicParseError] {.inline.} =
  proc inner(
      token: Token
  ): Result[string, BasicParseError] {.inline, gcsafe, noSideEffect.} =
    ok(token.qStr)

  expect parser, [tkQuotedString], (token: Token) => inner token

proc expectIdentOrString*(parser: Parser): Result[string, BasicParseError] {.inline.} =
  proc inner(
      token: Token
  ): Result[string, BasicParseError] {.inline, gcsafe, noSideEffect.} =
    case token.kind
    of tkIdent:
      return ok token.ident
    of tkQuotedString:
      return ok token.qStr
    else:
      discard

  expect parser, [tkIdent, tkQuotedString], (token: Token) => inner token

proc expectUrl*(parser: Parser): Result[string, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[string, BasicParseError] {.inline.} =
    case token.kind
    of tkUnquotedUrl:
      return ok token.uqUrl
    of tkFunction:
      if token.fnName.toLowerAscii() == "url":
        proc parseFn(input: Parser): Result[string, ParseError[BasicParseError]] =
          let str = input.expectString()
          if str.isOk:
            return ok(str.get())
          else:
            return err(ParseError[BasicParseError](kind: peBasic, basic: str.error()))

        # TODO: wtf is this and why does it work?
        let res = parser.parseNestedBlock(parseFn)
        if not res.isOk:
          return res.error().basic.err()
        else:
          return res.get().ok()
    else:
      discard

  expect parser, [tkUnquotedUrl, tkFunction], (token: Token) => inner token

proc expectUrlOrString*(parser: Parser): Result[string, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[string, BasicParseError] {.inline.} =
    case token.kind
    of tkUnquotedUrl:
      return ok token.uqUrl
    of tkQuotedString:
      return ok token.qStr
    of tkFunction:
      # TODO: same clusterfuck as above, move it into another function
      if token.fnName.toLowerAscii() == "url":
        proc parseFn(input: Parser): Result[string, ParseError[BasicParseError]] =
          let str = input.expectString()
          if str.isOk:
            return ok(str.get())
          else:
            return err(ParseError[BasicParseError](kind: peBasic, basic: str.error()))

        # TODO: wtf is this and why does it work?
        let res = parser.parseNestedBlock(parseFn)
        if not res.isOk:
          return res.error().basic.err()
        else:
          return res.get().ok()
    else:
      discard

  expect parser,
    [tkUnquotedUrl, tkQuotedString, tkFunction], (token: Token) => inner token

proc expectNumber*(parser: Parser): Result[float32, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[float32, BasicParseError] {.inline.} =
    case token.kind
    of tkNumber:
      return ok token.nValue
    else:
      discard

  expect parser, [tkNumber], (token: Token) => inner token

proc expectInt*(parser: Parser): Result[int32, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[int32, BasicParseError] {.inline.} =
    case token.kind
    of tkNumber:
      return ok token.nIntVal.get()
    else:
      discard

  expect parser, [tkNumber], (token: Token) => inner token

proc expectPercentage*(parser: Parser): Result[float32, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[float32, BasicParseError] {.inline.} =
    case token.kind
    of tkPercentage:
      return ok token.pUnitValue
    else:
      discard

  expect parser, [tkPercentage], (token: Token) => inner token

proc expectColon*(parser: Parser): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkColon], (token: Token) => inner token

proc expectSemicolon*(parser: Parser): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkSemicolon], (token: Token) => inner token

proc expectComma*(parser: Parser): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkComma], (token: Token) => inner token

proc expectDelim*(parser: Parser): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkDelim], (token: Token) => inner token

proc expectCurlyBracketBlock*(
    parser: Parser
): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkCurlyBracketBlock], (token: Token) => inner token

proc expectCloseCurlyBracket*(
    parser: Parser
): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkCloseCurlyBracket], (token: Token) => inner token

proc expectSquareBracketBlock*(
    parser: Parser
): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkSquareBracketBlock], (token: Token) => inner token

proc expectParenBlock*(parser: Parser): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkParenBlock], (token: Token) => inner token

proc expectFunction*(parser: Parser): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] {.inline.} =
    return ok()

  expect parser, [tkFunction], (token: Token) => inner token

proc expectFunctionMatching*(
    parser: Parser, name: string
): Result[void, BasicParseError] {.inline.} =
  proc inner(token: Token): Result[void, BasicParseError] =
    if token.fnName.toLowerAscii() == name:
      return ok()

  expect parser, [tkFunction], (token: Token) => inner token

proc expectNoErrorToken*(parser: Parser): Result[void, BasicParseError] {.inline.} =
  while true:
    let next = parser.nextIncludingWhitespaceAndComments()
    if next.isErr:
      return ok()

    let token = get next

    case token.kind
    of tkFunction:
      return ok()
    of tkParenBlock:
      return ok()
    of tkSquareBracketBlock:
      return ok()
    of tkCurlyBracketBlock:
      proc parseFn(parser: Parser): Result[void, ParseError[BasicParseError]] =
        let res = parser.expectNoErrorToken()
        if res.isErr:
          return err(ParseError[BasicParseError](kind: peBasic, basic: res.error()))
        else:
          return ok()

      let parsed = parser.parseNestedBlock(parseFn)
      if parsed.isErr:
        return err(parsed.error().basic)
      else:
        return ok()
    else:
      if token.isParseError():
        return err(parser.newBasicUnexpectedTokenError(deepCopy token))

export shared
