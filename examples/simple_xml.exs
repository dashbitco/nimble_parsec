# Run it from root with `mix run examples/simple_xml.exs`

defmodule SimpleXML do
  import NimbleParsec

  @doc """
  Parses a simple XML.

  It is meant to show NimbleParsec recursive features.
  It doesn't support attributes. The content of a tag is
  either another tag or a text node.
  """
  def parse(xml, opts \\ []) do
    opts = Keyword.put(opts, :context, %{tags: []})
    xml(xml, opts)
  end

  tag = ascii_string([?a..?z, ?A..?Z], min: 1)
  text = ascii_string([not: ?<], min: 1)

  opening_tag = ignore(string("<")) |> concat(tag) |> ignore(string(">"))
  closing_tag = ignore(string("</")) |> concat(tag) |> ignore(string(">"))

  defcombinatorp :node,
                 opening_tag
                 |> post_traverse(:store_tag_in_context)
                 |> repeat(
                   lookahead_not(string("</"))
                   |> choice([parsec(:node), text])
                 )
                 |> wrap()
                 |> concat(closing_tag)
                 |> post_traverse(:check_close_tag_and_emit_tag)

  defparsecp :xml, parsec(:node) |> eos()

  defp store_tag_in_context(_rest, [tag], %{tags: tags} = context, _line, _offset) do
    {[tag], %{context | tags: [tag | tags]}}
  end

  defp check_close_tag_and_emit_tag(_rest, [tag, [tag | contents]], context, _line, _offset) do
    context = update_in(context.tags, &tl/1)

    text_or_nodes =
      case contents do
        [text] -> text
        nodes -> nodes
      end

    {[{String.to_atom(tag), [], text_or_nodes}], context}
  end

  defp check_close_tag_and_emit_tag(_rest, [opening, [closing | _]], _context, _line, _offset) do
    {:error, "closing tag #{inspect(closing)} did not match opening tag #{inspect(opening)}"}
  end
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
