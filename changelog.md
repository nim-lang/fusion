## additions and changes

- Pattern matching implementation as described in nim-lang/RFCs#245

- Replace `Table` in names for types and procedures in `btreetables`
  with `Map` to avoid name clashes and confusion with `std/tables`
- Added module `pointers` containing `toUncheckedArray`.
- Added `filepermissions.chmod` and `filepermissions.fromFilePermissions`,
  convenience functions to change file permissions using Unix like octal file permissions.
