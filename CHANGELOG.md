# Changelog for NimbleParsec

## v0.2.0

  * Rename `literal/2` to `string/2`
  * Instead of `line` as a positive integer, we now track `{line, line_offset}` where `line` is the same as before and `line_offset` is the byte_offset after the new line
  * Instead of `column` as a positive integer, we now track `byte_offset` as a non-negative integer

## v0.1.0 (2018-03-02)

  * First release.