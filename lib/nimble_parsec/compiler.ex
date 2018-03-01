defmodule NimbleParsec.Compiler do
  @moduledoc false

  def compile(name, [], _opts) do
    raise ArgumentError, "cannot compile #{inspect(name)} with an empty parser combinator"
  end

  def compile(name, combinators, _opts) when is_list(combinators) do
    config = %{
      name: name,
      step: 0
    }

    {next, config} = build_next(config)

    combinators
    |> Enum.reverse()
    |> compile([], next, config)
  end

  defp compile([], defs, current, _config) do
    # TODO: Allow OK to be customized via config.
    # TODO: Allow ERROR to be customized via config.
    body =
      quote(do: {:ok, Enum.reverse(combinator__acc), rest, combinator__line, combinator__column})

    [build_def(current, quote(do: rest), [], body) | defs]
  end

  defp compile(combinators, defs, current, config) do
    {combinators, {match_def, catch_all_def, next, config}} =
      case take_bound_combinators(combinators) do
        {[combinator | combinators], [], [], [], [], _} ->
          {combinators, compile_unbound_combinator(combinator, current, config)}

        {combinators, inputs, guards, outputs, cursors, _} ->
          {combinators,
           compile_bound_combinator(inputs, guards, outputs, cursors, current, config)}
      end

    compile(combinators, [match_def, catch_all_def | defs], next, config)
  end

  ## Unbound combinators

  defp compile_unbound_combinator(combinator, _defs, _current) do
    raise "TODO: #{inspect(combinator)} not yet compilable"
  end

  ## Bound combinators

  # A bound combinator is a combinator where the number of inputs, guards,
  # outputs, cursor shifts are known at compilation time. We inline those bound
  # combinators into a single bitstring pattern for performance. Currently error
  # reporting will accuse the beginning of the bound combinator in case of errors
  # but such can be addressed if desired.

  defp compile_bound_combinator(inputs, guards, outputs, cursors, current, config) do
    {next, config} = build_next(config)
    acc = quote(do: unquote(outputs) ++ combinator__acc)
    {line, column} = apply_cursors(cursors, 0, 0, false)
    body = invoke_next(next, quote(do: rest), acc, line, column)

    arg = {:<<>>, [], inputs ++ [quote(do: rest :: binary)]}
    match_def = build_def(current, arg, guards, body)
    catch_all_def = build_catch_all(current)
    {match_def, catch_all_def, next, config}
  end

  defp apply_cursors([{:column, new_column} | cursors], line, column, column_reset?) do
    apply_cursors(cursors, line, column + new_column, column_reset?)
  end

  defp apply_cursors([{:line, new_line, new_column} | cursors], line, _column, _column_reset?) do
    apply_cursors(cursors, line + new_line, new_column, true)
  end

  defp apply_cursors([], line, column, column_reset?) do
    line_quoted =
      if line == 0 do
        quote(do: combinator__line)
      else
        quote(do: combinator__line + unquote(line))
      end

    column_quoted =
      if column_reset? do
        column
      else
        quote(do: combinator__column + unquote(column))
      end

    {line_quoted, column_quoted}
  end

  defp take_bound_combinators(combinators) do
    take_bound_combinators(combinators, [], [], [], [], 0)
  end

  defp take_bound_combinators(combinators, inputs, guards, outputs, cursors, counter) do
    with [combinator | combinators] <- combinators,
         {:ok, new_inputs, new_guards, new_outputs, new_cursors, new_counter} <-
           bound_combinator(combinator, counter) do
      take_bound_combinators(
        combinators,
        inputs ++ new_inputs,
        guards ++ new_guards,
        new_outputs ++ outputs,
        cursors ++ new_cursors,
        new_counter
      )
    else
      _ ->
        {combinators, inputs, guards, outputs, cursors, counter}
    end
  end

  defp bound_combinator({:compile_map, combinators, compile_fun, _runtime_fun}, counter) do
    case take_bound_combinators(combinators, [], [], [], [], counter) do
      {[], inputs, guards, outputs, cursors, counter} ->
        outputs = outputs |> Enum.reverse() |> compile_fun.() |> Enum.reverse()
        {:ok, inputs, guards, outputs, cursors, counter}

      {_, _, _, _, _, _} ->
        :error
    end
  end

  defp bound_combinator({:compile_bit_integer, ranges, modifiers}, counter) do
    {var, counter} = build_var(counter)

    input =
      case modifiers do
        [] -> var
        _ -> {:::, [], [var, Enum.reduce(modifiers, &{:-, [], [&2, &1]})]}
      end

    guards =
      for min..max <- ranges do
        quote(do: unquote(var) >= unquote(min) and unquote(var) <= unquote(max))
      end

    {:ok, [input], guards, [var], [{:column, 1}], counter}
  end

  defp bound_combinator({:literal, combinator}, counter) do
    cursor =
      case String.split(combinator, "\n") do
        [single] ->
          {:column, String.length(single)}

        [_ | _] = many ->
          column = many |> List.last() |> String.length()
          {:line, length(many) - 1, column + 1}
      end

    {:ok, [combinator], [], [combinator], [cursor], counter}
  end

  defp bound_combinator(_, _counter) do
    :error
  end

  ## Helpers

  defp build_next(%{name: name, step: step} = config) do
    {:"#{name}__#{step}", %{config | step: step + 1}}
  end

  defp invoke_next(next, rest, acc, line, column) do
    {next, [], [rest, acc, line, column]}
  end

  defp build_def(name, arg, guards, body) do
    args = quote(do: [unquote(arg), combinator__acc, combinator__line, combinator__column])

    guards =
      case guards do
        [] -> true
        _ -> Enum.reduce(guards, &{:and, [], [&2, &1]})
      end

    {name, args, guards, body}
  end

  defp build_catch_all(name) do
    args = quote(do: [rest, acc, line, column])
    body = quote(do: {:error, rest, line, column})
    {name, args, true, body}
  end

  defp build_var(counter) do
    {{:"x#{counter}", [], __MODULE__}, counter + 1}
  end
end
