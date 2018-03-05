defmodule NimbleParsec.Printer do
  @moduledoc """
  Functions for printing parser combinator code.
  """

  @doc """
  Prints parser combinator code.

  Receives the same options as `NimbleParsec.defparsec/3`.
  """
  @spec defparsec(atom(), NimbleParsec.t(), keyword()) :: iodata
  def defparsec(name, combinator, opts \\ []) do
    inline? = Keyword.get(opts, :inline, false)
    {defs, inline} = NimbleParsec.Compiler.compile(name, combinator, opts)
    [format_entry_point(name), ?\n | format_functions(defs, inline, inline?)]
  end

  @doc """
  Prints private parser combinator code.

  Receives the same options as `NimbleParsec.defparsecp/3`.
  """
  @spec defparsecp(atom(), NimbleParsec.t(), keyword()) :: iodata
  def defparsecp(name, combinator, opts \\ []) do
    inline? = Keyword.get(opts, :inline, false)
    {defs, inline} = NimbleParsec.Compiler.compile(name, combinator, opts)
    format_functions(defs, inline, inline?)
  end

  @doc false
  def format_entry_point(name) do
    {doc, spec, def} = NimbleParsec.Compiler.entry_point(name)

    """
    @doc "\""
    #{doc}
    "\""
    @spec #{Macro.to_string(spec)}
    #{format_function(:def, def)}
    """
  end

  @doc false
  def format_functions(defs, inline, inline?) do
    functions = Enum.map(defs, &format_function(:defp, &1))
    inline = if inline?, do: "@compile {:inline, #{inline}}\n", else: ""
    [inline | functions]
  end

  defp format_function(kind, {name, args, guards, body}) do
    signature = Macro.to_string(quote(do: unquote(name)(unquote_splicing(args))))

    if guards == true do
      """
      #{kind} #{signature} do
        #{Macro.to_string(body)}
      end

      """
    else
      """
      #{kind} #{signature} when #{Macro.to_string(guards)} do
        #{Macro.to_string(body)}
      end

      """
    end
  end

  @doc false
  def print_string(string) do
    string
    |> prepend_imports()
    |> EEx.eval_string()
    |> maybe_format_code()
  end

  @doc false
  def print_file(path) do
    path
    |> File.read!()
    |> prepend_imports()
    |> EEx.eval_string([], file: path)
    |> maybe_format_code()
  end

  defp prepend_imports(code) do
    "<% import NimbleParsec, except: [defparsec: 2, defparsec: 3, defparsecp: 2, defparsecp: 3] %>" <>
      "<% import NimbleParsec.Printer, only: [defparsec: 2, defparsec: 3, defparsecp: 2, defparsecp: 3] %>" <>
      code
  end

  defp maybe_format_code(code) do
    if Code.ensure_loaded?(Code) and function_exported?(Code, :format_string!, 1) do
      code
      |> IO.iodata_to_binary()
      |> Code.format_string!()
    else
      code
    end
  end
end
