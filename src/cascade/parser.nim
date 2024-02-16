import std/options

import ./[shared]

type
  BlockType* = enum
    btParenthesis
    btSquareBracket
    btCurlyBracket

  ParserState* = object
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

  BasicParseError* = object
    case kind*: BasicParseErrorKind
    of bpUnexpectedToken:
      token*: Token
    of bpAtRuleInvalid:
      rule*: string
    else:
      discard

    location*: SourceLocation

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
