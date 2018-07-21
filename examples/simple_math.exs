# Run it from root with `mix run examples/simple_math.exs`

# Language to describe basic math expression with two operands: + and *
defmodule SimpleMath do
  import NimbleParsec

  # nat    := 0 | 1 | 2 | ...
  # factor := ( expr ) | nat
  # term   := factor * term | factor
  # expr   := term + expr | term

  nat = integer(min: 1)

  factor =
    choice([
      ignore(ascii_char([?(]))
      |> concat(parsec(:expr))
      |> ignore(ascii_char([?)])),
      nat
    ])

  # Recursive definitions require using defparsec with the parsec combinator

  defcombinatorp :term,
                 choice([
                   factor
                   |> ignore(ascii_char([?*]))
                   |> concat(parsec(:term))
                   |> tag(:mul),
                   factor
                 ])

  defcombinatorp :expr,
                 choice([
                   parsec(:term)
                   |> ignore(ascii_char([?+]))
                   |> concat(parsec(:expr))
                   |> tag(:plus),
                   parsec(:term)
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
  IO.inspect(SimpleMath.parse(input))
  IO.puts("")
end
