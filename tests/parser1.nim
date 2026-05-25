import std/unittest
import results, stylus/parser

var input = newParserInput(
"""
h1 {
  this-is-not: "a-real-attribute";
}
  """
)

let parserObj = newParser(input)

test "basic parsing":
  assert parserObj.expectIdent().get() == "h1"
  assert parserObj.expectCurlyBracketBlock().isOk
  assert parserObj.expectIdentMatching("this-is-not").get() == "this-is-not"
  assert parserObj.expectColon().isOk
  assert parserObj.expectString().get() == "a-real-attribute"
