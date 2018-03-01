defmodule NimbleParsec.Compiler do
  @moduledoc false

  def compile(name, [], _opts) do
    raise ArgumentError, "cannot compile #{inspect(name)} with an empty parser combinator"
  end

  def compile(name, combinators, _opts) when is_list(combinators) do
    config = %{
      name: name
    }

    {next, step} = build_next(0, config)

    combinators
    |> Enum.reverse()
    |> compile([], next, step, config)
  end

  defp compile([], defs, current, _step, _config) do
    # TODO: Allow OK to be customized via config.
    # TODO: Allow ERROR to be customized via config.
    body =
      quote(do: {:ok, Enum.reverse(combinator__acc), rest, combinator__line, combinator__column})

    [build_def(current, quote(do: rest), [], body) | defs]
  end

  defp compile(combinators, defs, current, step, config) do
    {next_combinators, used_combinators, {match_def, next, step}} =
      case take_bound_combinators(combinators) do
        {[combinator | combinators], [], [], [], [], [], _} ->
          {combinators, [combinator], compile_unbound_combinator(combinator, current, step, config)}

        {combinators, inputs, guards, outputs, cursors, acc, _} ->
          {combinators, Enum.reverse(acc),
           compile_bound_combinator(inputs, guards, outputs, cursors, current, step, config)}
      end

    catch_all_def = build_catch_all(current, error_reason(used_combinators))
    compile(next_combinators, [match_def, catch_all_def | defs], next, step, config)
  end

  ## Unbound combinators

  defp compile_unbound_combinator(combinator, _defs, _step, _current) do
    raise "TODO: #{inspect(combinator)} not yet compilable"
  end

  ## Bound combinators

  # A bound combinator is a combinator where the number of inputs, guards,
  # outputs, cursor shifts are known at compilation time. We inline those bound
  # combinators into a single bitstring pattern for performance. Currently error
  # reporting will accuse the beginning of the bound combinator in case of errors
  # but such can be addressed if desired.

  defp compile_bound_combinator(inputs, guards, outputs, cursors, current, step, config) do
    {next, step} = build_next(step, config)
    acc = quote(do: unquote(outputs) ++ combinator__acc)
    {line, column} = apply_cursors(cursors, 0, 0, false)
    body = invoke_next(next, quote(do: rest), acc, line, column)

    arg = {:<<>>, [], inputs ++ [quote(do: rest :: binary)]}
    match_def = build_def(current, arg, guards, body)
    {match_def, next, step}
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
    take_bound_combinators(combinators, [], [], [], [], [], 0)
  end

  defp take_bound_combinators(combinators, inputs, guards, outputs, cursors, acc, counter) do
    with [combinator | combinators] <- combinators,
         {:ok, new_inputs, new_guards, new_outputs, new_cursors, new_counter} <-
           bound_combinator(combinator, counter) do
      take_bound_combinators(
        combinators,
        inputs ++ new_inputs,
        guards ++ new_guards,
        new_outputs ++ outputs,
        cursors ++ new_cursors,
        [combinator | acc],
        new_counter
      )
    else
      _ ->
        {combinators, inputs, guards, outputs, cursors, acc, counter}
    end
  end

  defp bound_combinator({:literal, binary}, counter) do
    cursor =
      case String.split(binary, "\n") do
        [single] ->
          {:column, String.length(single)}

        [_ | _] = many ->
          column = many |> List.last() |> String.length()
          {:line, length(many) - 1, column + 1}
      end

    {:ok, [binary], [], [binary], [cursor], counter}
  end

  defp bound_combinator({:label, combinators, _labels}, counter) do
    case take_bound_combinators(combinators, [], [], [], [], [], counter) do
      {[], inputs, guards, outputs, cursors, _, counter} ->
        {:ok, inputs, guards, outputs, cursors, counter}

      {_, _, _, _, _, _} ->
        :error
    end
  end

  defp bound_combinator({:compile_bit_integer, ranges, modifiers}, counter) do
    {var, counter} = build_var(counter)
    input = apply_bit_modifiers(var, modifiers)
    guards = ranges_to_guards(var, ranges)
    {:ok, [input], guards, [var], [{:column, 1}], counter}
  end

  defp bound_combinator({:compile_map, combinators, compile_fun, _runtime_fun}, counter) do
    case take_bound_combinators(combinators, [], [], [], [], [], counter) do
      {[], inputs, guards, outputs, cursors, _, counter} ->
        {:ok, inputs, guards, compile_fun.(outputs), cursors, counter}

      {_, _, _, _, _, _} ->
        :error
    end
  end

  defp bound_combinator(_, _counter) do
    :error
  end

  ## Label and error handling

  defp error_reason(combinators) do
    "expected " <> labels(combinators)
  end

  defp labels(combinators) do
    Enum.map_join(combinators, ", followed by ", &label/1)
  end

  defp label({:literal, binary}) do
    "a literal #{inspect(binary)}"
  end

  defp label({:label, _document, label}) do
    label
  end

  defp label({:compile_bit_integer, [], _modifiers}) do
    "a byte"
  end

  defp label({:compile_bit_integer, ranges, _modifiers}) do
    inspected = Enum.map_join(ranges, ", ", &inspect_byte_range/1)
    "a byte in the #{pluralize(length(ranges), "range", "ranges")} #{inspected}"
  end

  defp label({:compile_map, combinators, _, _}) do
    labels(combinators)
  end

  defp inspect_byte_range(min..max) do
    if ascii?(min) and ascii?(max) do
      <<??, min, ?., ?., ??, max>>
    else
      "#{Integer.to_string(min)}..#{Integer.to_string(max)}"
    end
  end

  defp ascii?(char), do: char >= 32 and char <= 126

  defp pluralize(1, singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  ## Helpers

  defp ranges_to_guards(var, ranges) do
    for min..max <- ranges do
      cond do
        min < max -> quote(do: unquote(var) >= unquote(min) and unquote(var) <= unquote(max))
        min > max -> quote(do: unquote(var) >= unquote(max) and unquote(var) <= unquote(min))
        true -> quote(do: unquote(var) === unquote(min))
      end
    end
  end

  defp apply_bit_modifiers(expr, modifiers) do
    case modifiers do
      [] -> expr
      _ -> {:::, [], [expr, Enum.reduce(modifiers, &{:-, [], [&2, &1]})]}
    end
  end

  defp build_next(step, %{name: name}) do
    {:"#{name}__#{step}", step + 1}
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

  defp build_catch_all(name, reason) do
    args = quote(do: [rest, acc, line, column])
    body = quote(do: {:error, unquote(reason), rest, line, column})
    {name, args, true, body}
  end

  defp build_var(counter) do
    {{:"x#{counter}", [], __MODULE__}, counter + 1}
  end
end
