# Run it from root with `mix run examples/simple_math.exs`
# Run it as `mix run examples/simple_math.exs --generate` to see some generator outputs

# Language to describe basic math expression with two operands: + and *
defmodule SimpleMath do
  import NimbleParsec

  # nat    := 0 | 1 | 2 | ...
  # factor := ( expr ) | nat
  # term   := factor * term | factor
  # expr   := term + expr | term

  nat = integer(min: 1)

  factor =
    empty()
    |> choice(
      [
        ignore(ascii_char([?(]))
        |> concat(parsec(:expr))
        |> ignore(ascii_char([?)])),
        nat
      ],
      gen_weights: [1, 3]
    )

  # Recursive definitions require using defparsec with the parsec combinator
  #
  # Note we use export_metadata: true for the generator functionality,
  # it is not required if you are only doing parsing.

  defcombinatorp :term,
                 empty()
                 |> choice(
                   [
                     factor
                     |> ignore(ascii_char([?*]))
                     |> concat(parsec(:term))
                     |> tag(:mul),
                     factor
                   ],
                   gen_weights: [1, 3]
                 ),
                 export_metadata: true

  defcombinatorp :expr,
                 empty()
                 |> choice(
                   [
                     parsec(:term)
                     |> ignore(ascii_char([?+]))
                     |> concat(parsec(:expr))
                     |> tag(:plus),
                     parsec(:term)
                   ],
                   gen_weights: [1, 3]
                 ),
                 export_metadata: true

  defparsec :parse, parsec(:expr), export_metadata: true
end

if "--generate" in System.argv() do
  IO.puts(NimbleParsec.generate(NimbleParsec.parsec({SimpleMath, :parse})))
  IO.puts(NimbleParsec.generate(NimbleParsec.parsec({SimpleMath, :parse})))
  IO.puts(NimbleParsec.generate(NimbleParsec.parsec({SimpleMath, :parse})))
else
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
end
