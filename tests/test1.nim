import cascade, pretty

const src = """
/* Hello CSS! 
   This is some basic CSS code to test Cascade's tokenizer!
   Currently, it can support comments, functions, numericals, and more!
*/
body {
  /* Sets the color to a linear gradient */
  color: linear-gradient(45deg, red, blue);
  
  /* Sets `awesomeness` to 45% */
  awesomemess: 45%;

  /* Sets the background image to a URL that our browser will fetch */
  background-image: url(https://ferus.org/assets/h2kaofwlweqeJWmq.png);
}
"""

let tokenizer = tokenizer(src)

while not tokenizer.isEof():
  let tok = tokenizer.nextToken()
  print tok
