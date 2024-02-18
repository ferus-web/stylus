import cascade, pretty

const src = """
/* Hello CSS! */
h1 {
  /* Sets the color to a linear gradient */
  color: linear-gradient(45deg, red, blue);
  size: 45.3px;
}
"""

let tokenizer = tokenizer(src)

while not tokenizer.isEof():
  let tok = tokenizer.nextToken()
  print tok
