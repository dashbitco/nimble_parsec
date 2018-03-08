# Changelog for NimbleParsec

## v0.2.2 (2018-03-08)

### Bug fix

  * Ensure multiple runtime traversals compose

## v0.2.1 (2018-03-04)

### Bug fix

  * Do not duplicate entries in integer/ascii_string/utf8_string

## v0.2.0 (2018-03-04)

### Enhancements

  * Support parser combinator contexts and return them in ok/error tuples
  * Add `ascii_string/3` and `utf_string/3` combinators
  * Add `lookahead/2` combinator
  * Rename `literal/2` to `string/2`
  * Add `line/2` and `byte_offset/2` combinators
  * The MFA given to `repeat_while/3` now receives the accumulator, context, line, and byte_offset values and must return `{:cont, context} | {:halt, context}`
  * The MFA given to `quoted_repeat_while/3` now receives the accumulator, context, line, and byte_offset values and must return `{:cont, context} | {:halt, context}`
  * The MFA given to `traverse/3` now receives the rest, accumulator, context, line, and byte_offset values and must return `{acc, context} | {:error, reason}`
  * The MFA given to `quoted_traverse/3` now receives the ASTs for the rest, accumulator, context, line, and byte_offset and must return `{acc, context} | {:error, reason}`
  * Instead of `line` as a positive integer, we now track `{line, line_offset}` where `line` is the same as before and `line_offset` is the byte offset after the new line
  * Instead of `column` as a positive integer, we now track `byte_offset` as a non-negative integer

## v0.1.0 (2018-03-02)

  * First release.