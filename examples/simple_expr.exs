# Run it from root with `mix run examples/simple_expr.exs`

# Language to describe basic math expression with two operands: [+, *]
defmodule SimpleExpr do
  import NimbleParsec

  #
  # expr   := term + expr | term
  # term   := factor * term | factor
  # factor := ( expr ) | nat
  # nat    := 0 | 1 | 2 | ...
  #

  nat = integer(min: 1) |> unwrap_and_tag(:nat)

  defparsecp :expr,
             choice([
               parsec(:term)
               |> ignore(ascii_char([?+]))
               |> concat(parsec(:expr))
               |> tag(:plus),
               parsec(:term)
             ])

  defparsecp :term,
             choice([
               parsec(:factor)
               |> ignore(ascii_char([?*]))
               |> concat(parsec(:term))
               |> tag(:mul),
               parsec(:factor)
             ])

  defparsecp :factor,
             choice([
               ignore(ascii_char([?(]))
               |> concat(parsec(:expr))
               |> wrap()
               |> ignore(ascii_char([?)])),
               nat
             ])

  defparsec :parse, parsec(:expr)
end

inputs = [
  "5+3",
  "1*2",
  "(1*7)+9",
  "1+1+0+3*71",
  "(1982)",
  "0",
  "1+(2+(3*7))"
]

for input <- inputs do
  IO.puts(input)
  IO.inspect(SimpleExpr.parse(input))
  IO.puts("")
end
