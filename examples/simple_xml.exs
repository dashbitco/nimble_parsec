# Run it from root with `mix run examples/simple_xml.exs`

defmodule SimpleXML do
  import NimbleParsec

  @doc """
  Parses a simple XML.

  It is meant to show NimbleParsec recursive features.
  It doesn't support attributes. The content of a tag is
  either another tag or a text node.
  """
  defparsec :parse, parsec(:node) |> eos()

  tag = ascii_string([?a..?z, ?A..?Z], min: 1)
  text = ascii_string([not: ?<], min: 1)

  opening_tag = ignore(string("<")) |> concat(tag) |> ignore(string(">"))
  closing_tag = ignore(string("</")) |> concat(tag) |> ignore(string(">"))

  defcombinatorp :node,
                 opening_tag
                 |> repeat(lookahead_not(string("</")) |> choice([parsec(:node), text]))
                 |> wrap()
                 |> concat(closing_tag)
                 |> post_traverse(:match_and_emit_tag)

  defp match_and_emit_tag(_rest, [tag, [tag, text]], context, _line, _offset),
    do: {[{String.to_atom(tag), [], text}], context}

  defp match_and_emit_tag(_rest, [tag, [tag | nodes]], context, _line, _offset),
    do: {[{String.to_atom(tag), [], nodes}], context}

  defp match_and_emit_tag(_rest, [opening, [closing | _]], _context, _line, _offset),
    do: {:error, "closing tag #{inspect(closing)} did not match opening tag #{inspect(opening)}"}
end

inputs = [
  "<foo>bar</foo>",
  "<foo><bar>baz</bar></foo>",
  "<foo><bar>one</bar><bar>two</bar></foo>",
  "<>bar</>",
  "<foo>bar</baz>",
  "<foo>bar</foo>oops",
  "<foo>bar"
]

for input <- inputs do
  IO.puts(input)
  IO.inspect(SimpleXML.parse(input))
  IO.puts("")
end
