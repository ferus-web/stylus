import results, pretty, stylus/parser

var input = newParserInput(
  """
h1 {
  this-is-not: "a-real-attribute";
}
  """
)

let parserObj = newParser(input)

echo parserObj.expectIdent()
echo parserObj.expectCurlyBracketBlock()
echo parserObj.expectIdentMatching("this-is-not")
echo parserObj.expectColon()
echo parserObj.expectString().get() == "a-real-attribute"
