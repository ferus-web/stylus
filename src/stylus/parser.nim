import std/options, results

import ./[shared, tokenizer]

type
  BlockType* = enum
    btParenthesis
    btSquareBracket
    btCurlyBracket

  ParserState* = ref object
    position*: uint
    currentLineStartPos*: uint
    currentLineNumber*: uint
    atStartOf*: Option[BlockType]

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

  Delimiters* = ref object
    bits*: byte

  Parser* = ref object
    input*: ParserInput
    atStartOf*: Option[BlockType]
    stopBefore*: Delimiters

let
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

proc newParser*(
  input: ParserInput
): Parser {.inline.} =
  Parser(
    input: input,
    atStartOf: none(BlockType),
    stopBefore: DelimNone
  )

proc currLine*(parser: Parser): string {.inline.} =
  parser.input.tokenizer.currentSourceLine()

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

proc basicUnexpectedTokenError*(token: Token): BasicParseError {.inline.} =
  BasicParseError(kind: bpUnexpectedToken, token: token)

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
