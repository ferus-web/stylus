# Stylus -- a CSS level 3 parser written in pure Nim
Stylus is mostly a rewrite of `rust-cssparser` in Nim (for now) and it aims to support the entire CSS3 specification. \
The tokenizer and parser are *mostly* done, but a few functions and missing.

# The Parser
The parser is _not_ designed to hand over a proper rule vector to you - you're supposed to build one yourself! The parser simply gives you the tools to gracefully parse a CSS3 stylesheet. This allows for a flexible range of syntax that might not be prescribed in the actual spec.

# Users
Stylus is currently used by:
- ferus, a web engine written in Nim ([here](https://github.com/ferus-web/ferus))
- figuro, a UI toolkit for Nim ([here](https://github.com/elcritch/figuro))

Have a project that uses Stylus? Let me know!
