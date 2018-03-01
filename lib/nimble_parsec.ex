# TODO: map at runtime
# TODO: map at compile time
# TODO: runtime_composition()
# TODO: integer()
# TODO: implement ignore / replace in favor of compile time map
# TODO: many()
# TODO: choice()
# TODO: implement integer(min, min) in terms of the constructs above
# TODO: Good error messages
# TODO: Docs

defmodule NimbleParsec do
  defguardp is_combinator(doc) when is_list(doc)

  defmacro defparsec(name, combinator, opts \\ []) do
    quote bind_quoted: [name: name, combinator: combinator, opts: opts] do
      def unquote(name)(binary, opts \\ []) when is_binary(binary) do
        unquote(:"#{name}__0")(binary, [], 1, 1)
      end

      for {name, args, guards, body} <- NimbleParsec.Compiler.compile(name, combinator, opts) do
        defp unquote(name)(unquote_splicing(args)) when unquote(guards), do: unquote(body)

        # IO.puts(Macro.to_string(quote do
        #   defp unquote(name)(unquote_splicing(args)) when unquote(guards), do: unquote(body)
        # end))
      end
    end
  end

  def empty() do
    []
  end

  @doc """
  Defines an integer combinator with `min` and `max` length.

  ## Examples

      defmodule MyParser do
        defparsec :two_digits_integer, integer(2, 2)
      end

      MyParser.two_digits_integer("123")
      #=> {:ok, [12], "3", 1, 3}

      MyParser.two_digits_integer("1a3")
      #=> {:error, "1a3", 1, 3}

  """
  def integer(combinator \\ empty(), min, max)

  def integer(combinator, size, size)
      when is_integer(size) and size > 0 and is_combinator(combinator) do
    [{:integer, size} | combinator]
  end

  # def integer(combinator, min, max)
  #     when is_integer(min) and min > 0 and is_integer(max) and max >= min and
  #            is_combinator(combinator) do
  #   [{:integer, min, max} | combinator]
  # end

  def literal(combinator \\ empty(), value) when is_combinator(combinator) and is_binary(value) do
    [{:literal, value} | combinator]
  end

  def ignore(combinator \\ empty(), to_ignore)
      when is_combinator(combinator) and is_combinator(to_ignore) do
    [{:ignore, to_ignore} | combinator]
  end
end
