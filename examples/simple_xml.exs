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

    case xml(xml, opts) do
      {:ok, acc, "", %{tags: []}, _line, _offset} ->
        {:ok, acc}

      {:ok, _, rest, %{tags: []}, line, offset} ->
        {:error, "document continued after last closing tag", rest, line, offset}

      {:ok, _, rest, %{tags: [tag | _]}, line, offset} ->
        {:error, "tag #{inspect(tag)} was not closed", rest, line, offset}

      {:error, reason, rest, _context, line, offset} ->
        {:error, reason, rest, line, offset}
    end
  end

  tag = ascii_string([?a..?z, ?A..?Z], min: 1)
  text = ascii_string([not: ?<], min: 1)

  opening_tag =
    ignore(string("<"))
    |> concat(tag)
    |> ignore(string(">"))

  closing_tag =
    ignore(string("</"))
    |> concat(tag)
    |> ignore(string(">"))

  defparsecp :xml,
             opening_tag
             |> traverse(:store_tag_in_context)
             |> repeat_until(
               choice([
                 parsec(:xml),
                 text
               ]),
               [string("</")]
             )
             |> wrap()
             |> concat(closing_tag)
             |> traverse(:check_close_tag_and_emit_tag)

  defp store_tag_in_context(_rest, [tag], %{tags: tags} = context, _line, _offset) do
    {[tag], %{context | tags: [tag | tags]}}
  end

  defp check_close_tag_and_emit_tag(_rest, acc, context, _line, _offset) do
    [closing, [opening | contents]] = acc

    if closing == opening do
      context = update_in(context.tags, &tl/1)

      element =
        case contents do
          [text] -> {String.to_atom(opening), [], text}
          nodes -> {String.to_atom(opening), [], nodes}
        end

      {[element], context}
    else
      {:error, "closing tag #{inspect(closing)} did not match opening tag #{inspect(opening)}"}
    end
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
