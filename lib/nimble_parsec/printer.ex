defmodule NimbleParsec.Printer do
  @moduledoc """
  Functions for printing parser combinator code.
  """

  @doc """
  Prints parser combinator code.

  Receives the same options as `NimbleParsec.defparsec/3`.
  """
  @spec defparsec(atom(), NimbleParsec.t(), keyword()) :: String.t()
  def defparsec(name, combinator, opts \\ []) do
    inline? = Keyword.get(opts, :inline, false)
    {defs, inline} = NimbleParsec.Compiler.compile(name, combinator, opts)
    print_entry_point(name) <> print_functions(defs, inline?, inline)
  end

  @doc """
  Prints private parser combinator code.

  Receives the same options as `NimbleParsec.defparsecp/3`.
  """
  @spec defparsecp(atom(), NimbleParsec.t(), keyword()) :: String.t()
  def defparsecp(name, combinator, opts \\ []) do
    inline? = Keyword.get(opts, :inline, false)
    {defs, inline} = NimbleParsec.Compiler.compile(name, combinator, opts)
    print_functions(defs, inline?, inline)
  end

  @doc false
  defp print_entry_point(name) do
    """
    def #{name}(binary, opts \\\\ []) when is_binary(binary) do
      line = Keyword.get(opts, :line, 1)
      offset = Keyword.get(opts, :byte_offset, 0)
      context = Map.new(Keyword.get(opts, :context, []))
      case #{name}__0(binary, [], [], context, {line, offset}, offset) do
        {:ok, acc, rest, context, line, offset} ->
          {:ok, :lists.reverse(acc), rest, context, line, offset}
        {:error, _, _, _, _, _} = error ->
          error
      end
    end

    """
  end

  @doc false
  def print_functions(defs, inline?, inline) do
    functions =
      for {name, args, guards, body} <- defs do
        """
        defp #{Macro.to_string(quote(do: unquote(name)(unquote_splicing(args))))}
             when #{Macro.to_string(guards)} do
          #{Macro.to_string(body)}
        end

        """
      end

    inline = if inline?, do: "@compile {:inline, #{inline}}\n", else: ""
    inline <> Enum.join(functions)
  end

  @doc false
  def print_string(string, bindings \\ [], eex_opts \\ []) do
    string
    |> prepend_imports()
    |> EEx.eval_string(bindings, eex_opts)
    |> maybe_format_code()
  end

  @doc false
  def print_file(path, bindings \\ [], eex_opts \\ []) do
    path
    |> File.read!()
    |> prepend_imports()
    |> EEx.eval_string(bindings, eex_opts)
    |> maybe_format_code()
  end

  defp prepend_imports(code) do
    """
    <%
    import NimbleParsec, except: [
             defparsec: 2,
             defparsec: 3,
             defparsecp: 2,
             defparsecp: 3
           ]
    import NimbleParsec.Printer, only: [
             defparsec: 2,
             defparsec: 3,
             defparsecp: 2,
             defparsecp: 3
           ]
    %>
    """ <> code
  end

  defp maybe_format_code(code) do
    if function_exported?(Code, :format_string!, 1) do
      code
      |> Code.format_string!()
      |> IO.iodata_to_binary()
    else
      code
    end
  end
end
