# Run it from root with `mix run examples/simple_language.exs`

# A language with integers and interpolated strings.
defmodule SimpleLanguage do
  import NimbleParsec

  interpolation =
    ignore(string(~S(#{)))
    |> parsec(:language)
    |> ignore(string(~S(})))
    |> unwrap_and_tag(:interpolation)

  integer =
    integer(min: 1)
    |> unwrap_and_tag(:integer)

  string =
    ascii_char([?"])
    |> repeat(
      lookahead_not(ascii_char([?"]))
      |> choice([
        ~S(\") |> string() |> replace(?"),
        ~S(\#) |> string() |> replace(?#),
        interpolation,
        utf8_char([])
      ])
    )
    |> ascii_char([?"])
    |> tag(:string)

  defparsec :language, choice([string, integer])
end

IO.inspect(SimpleLanguage.language(~S(123)))
IO.inspect(SimpleLanguage.language(~S("string with quotes \" inside")))
IO.inspect(SimpleLanguage.language(~S("string with #{123} interpolation inside")))
