## additions and changes

- Added pattern matching implementation as described in
  [nim-lang/RFCs#245](https://github.com/nim-lang/RFCs/issues/245)
- Added module `astdsl` that contains macro `buildAst`. That is a DSL for convenient
  construction of Nim ASTs.
- Added module `pointers` containing `toUncheckedArray`.
- Added `filepermissions.chmod` and `filepermissions.fromFilePermissions`,
  convenience functions to change file permissions using Unix like octal file permissions.
- Added module `scripting` providing `withDir` to switch the directory temporarily. This
  was previously only available in the `nimscript` module.




- Added `jsformdata` module for [`FormData`](https://developer.mozilla.org/en-US/docs/Web/API/FormData) for the JavaScript target.
- Added `jsheaders` module for [`Headers`](https://developer.mozilla.org/en-US/docs/Web/API/Headers) for the JavaScript target.
