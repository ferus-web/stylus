## Just some things to make it easier to write code with long functions. Not meant for use outside of stylus.

import std/options

proc `&`*[T](opt: Option[T]): bool {.inline.} =
  opt.isSome

proc `@`*[T](opt: Option[T]): T {.inline.} =
  opt.get()

proc unpack*[T](opt: Option[T], x: var T): bool {.inline.} =
  if &opt:
    x = unsafeGet(opt)
    return true

  false
