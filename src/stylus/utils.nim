import std/options

proc `&`*[T](opt: Option[T]): bool {.inline.} =
  opt.isSome

proc `@`*[T](opt: Option[T]): T {.inline.} =
  opt.get()
