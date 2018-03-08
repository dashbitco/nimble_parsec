defmodule NimbleParsec.Recorder do
  @moduledoc false

  @name __MODULE__

  @doc """
  Starts the recorder server.
  """
  def start_link(_opts) do
    Agent.start_link(fn -> %{} end, name: @name)
  end

  @doc """
  Stops the recorder server.
  """
  def stop() do
    Agent.stop(@name)
  end

  @doc """
  Records the given call and potentially debugs it.
  """
  def record(module, kind, name, defs, inline, opts) do
    inline? = Keyword.get(opts, :inline, false)

    if Keyword.get(opts, :debug, false) do
      IO.puts(format_defs(defs, inline, inline?))
    end

    if Process.whereis(@name) do
      Agent.update(@name, fn state ->
        update_in(state[module], &[{kind, name, defs, inline, inline?} | &1 || []])
      end)
    end

    :ok
  end

  defp format_entry_point(name) do
    {doc, spec, def} = NimbleParsec.Compiler.entry_point(name)

    """
    @doc "\""
    #{doc}
    "\""
    @spec #{Macro.to_string(spec)}
    #{format_def(:def, def)}
    """
  end

  defp format_defs(defs, inline, inline?) do
    functions = Enum.map(defs, &format_def(:defp, &1))
    inline = if inline?, do: "@compile {:inline, #{inline}}\n", else: ""
    [inline | functions]
  end

  defp format_def(kind, {name, args, guards, body}) do
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

  @doc """
  Replays recorded parsers on the given content.
  """
  def replay(contents, id) when is_binary(contents) do
    contents
    |> inject_recorded(id, Agent.get(@name, & &1))
    |> maybe_format_code()
  end

  defp inject_recorded(contents, id, recorded) do
    Enum.reduce(recorded, contents, fn {module, entries}, acc ->
      marker = "# parsec:#{inspect(module)}"

      case String.split(acc, marker) do
        [pre, _middle, pos] ->
          replacement = Enum.map(entries, &format_recorded/1)
          IO.iodata_to_binary([pre, replacement, pos])

        _ ->
          raise ArgumentError, "could not find marker #{inspect(marker)} on #{inspect(id)}"
      end
    end)
  end

  defp format_recorded({kind, name, defs, inline, inline?}) do
    entry_point =
      if kind == :def do
        format_entry_point(name)
      else
        []
      end

    [entry_point | format_defs(defs, inline, inline?)]
  end

  defp maybe_format_code(code) do
    if Code.ensure_loaded?(Code) and function_exported?(Code, :format_string!, 1) do
      [Code.format_string!(code) | "\n"]
    else
      code
    end
  end
end
