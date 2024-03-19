# ILOVECIRCULARIMPORTSILOVECIRCULARIMPORTSILOVECIRCULARIMPORTSILOVECIRCULARIMPORTSILOVECIRCULARIMPORTS
## This file has everything that is shared by the parser and tokenizer
import std/options

type
  SourcePosition* = uint ## A position from the start of the input, counted in characters

  SourceLocation* = object ## The line number
    line*: uint32

    ## The column number within a line
    column*: uint32

  TokenKind* = enum
    tkIdent
    tkAtKeyword
    tkHash
    tkIDHash
    tkQuotedString
    tkUnquotedUrl
    tkDelim
    tkNumber
    tkPercentage
    tkDimension
    tkWhiteSpace
    tkComment
    tkColon
    tkSemicolon
    tkComma
    tkIncludeMatch
    tkDashMatch
    tkPrefixMatch
    tkSuffixMatch
    tkSubstringMatch
    tkCDO
    tkCDC
    tkFunc
    tkParenBlock
    tkSquareBracketBlock
    tkCurlyBracketBlock
    tkBadUrl
    tkBadString
    tkCloseParen
    tkFunction
    tkCloseSquareBracket
    tkCloseCurlyBracket

  SeenStatus* = enum
    ssDontCare
    ssLooking
    ssSeenAtleastOne

const
  PARSE_ERRORS = [
    tkBadUrl, tkBadString, tkCloseParen, tkCloseSquareBracket, tkCloseCurlyBracket
  ]

type
  Token* = ref object
    case kind*: TokenKind
    of tkIdent:
      ident*: string
    of tkAtKeyword:
      at*: string
    of tkHash:
      hash*: string
    of tkIDHash:
      idHash*: string
    of tkQuotedString:
      qStr*: string
    of tkUnquotedUrl:
      uqUrl*: string
    of tkDelim:
      delim*: char
    of tkNumber:
      nHasSign*: bool
      nValue*: float32
      nIntVal*: Option[int32]
    of tkPercentage:
      pHasSign*: bool
      pUnitValue*: float32

      pIntVal*: Option[int32]
    of tkDimension:
      dHasSign*: bool
      dValue*: float32
      dIntVal*: Option[int32]
      unit*: string
    of tkWhitespace:
      wsStr*: string
    of tkComment:
      comment*: string
    of tkFunction:
      fnName*: string
    of tkBadUrl:
      badUrl*: string
    of tkBadString:
      badString*: string
    else:
      discard

  BlockType* = enum
    btParenthesis
    btSquareBracket
    btCurlyBracket

  ParserState* = ref object
    position*: uint
    currentLineStartPos*: uint
    currentLineNumber*: uint
    atStartOf*: Option[BlockType]

proc `$`*(token: Token): string =
  # TODO: complete this!
  case token.kind
  of tkIdent:
    token.ident
  of tkAtKeyword:
    '@' & token.at
  of tkHash:
    token.hash
  of tkIDHash:
    token.idHash
  of tkQuotedString:
    '"' & token.qStr & '"'
  of tkUnquotedUrl:
    token.uqUrl
  of tkDelim:
    $token.delim
  of tkNumber:
    if token.nIntVal.isSome:
      return $token.nIntVal
    else:
      return $token.nValue
  else: ""

proc isParseError*(token: Token): bool {.inline, noSideEffect, gcsafe.} =
  token.kind notin PARSE_ERRORS
