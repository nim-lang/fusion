## additions and changes

- Added module `astdsl` that contains macro `buildAst`. That is a DSL for convenient
  construction of Nim ASTs.
- Added module `pointers` containing `toUncheckedArray`.
- Added `filepermissions.chmod` and `filepermissions.fromFilePermissions`,
  convenience functions to change file permissions using Unix like octal file permissions.
- Added module `scripting` providing `withDir` to switch the directory temporarily. This
  was previously only available in the `nimscript` module.



- Added `jsxmlhttprequest` module for [`XMLHttpRequest`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest) for the JavaScript target.
