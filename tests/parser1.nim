import results, stylus/parser

var input = newParserInput(
  """
h1 {
  this-is-not: "a-real-attribute";
}
  """
)

let parserObj = newParser(input)
