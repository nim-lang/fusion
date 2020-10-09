## additions and changes

- Added module `astdsl` that contains macro `buildAst`. That is a DSL for convenient
  construction of Nim ASTs.
- Added module `pointers` containing `toUncheckedArray`.
- Added `filepermissions.chmod` and `filepermissions.fromFilePermissions`,
  convenience functions to change file permissions using Unix like octal file permissions.
- Added module `scripting` providing `withDir` to switch the directory temporarily. This
  was previously only available in the `nimscript` module.
- Added module `pointers` containing `toUncheckedArray`
- Add `enummaps` module which enables a DRY and flexible syntax to attach metadata to enums.
