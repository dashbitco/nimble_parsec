defmodule NimbleParsec.Compiler do
  @moduledoc false
  @arity 5

  def compile(name, [], _opts) do
    raise ArgumentError, "cannot compile #{inspect(name)} with an empty parser combinator"
  end

  def compile(name, combinators, _opts) when is_list(combinators) do
    config = %{
      catch_all: nil,
      labels: [],
      name: name,
      stack_depth: 0
    }

    {next, step} = build_next(0, config)

    {defs, inline, last, _step} =
      combinators
      |> Enum.reverse()
      |> compile([], [], next, step, config)

    {Enum.reverse([compile_ok(last) | defs]), [{last, @arity} | inline]}
  end

  defp compile_ok(current) do
    body =
      quote do
        {:ok, :lists.reverse(combinator__acc), rest, combinator__line, combinator__column}
      end

    build_def(current, quote(do: rest), [], body)
  end

  defp compile([], defs, inline, current, step, _config) do
    {defs, inline, current, step}
  end

  defp compile([{:update, key, fun} | combinators], defs, inline, current, step, config) do
    compile(combinators, defs, inline, current, step, Map.update!(config, key, fun))
  end

  defp compile(combinators, defs, inline, current, step, config) do
    {next_combinators, used_combinators, {new_defs, new_inline, next, step, catch_all}} =
      case take_bound_combinators(combinators) do
        {[combinator | combinators], [], [], [], [], _, _} ->
          case combinator do
            {:label, label_combinators, label} ->
              pre_combinators = [{:update, :labels, &[label | &1]} | label_combinators]
              pos_combinators = [{:update, :labels, &tl(&1)} | combinators]

              {pre_combinators ++ pos_combinators, [combinator],
               {[], [], current, step, :catch_none}}

            _ ->
              {combinators, [combinator],
               compile_unbound_combinator(combinator, current, step, config)}
          end

        {combinators, inputs, guards, outputs, acc, cursor, _} ->
          {combinators, Enum.reverse(acc),
           compile_bound_combinator(inputs, guards, outputs, cursor, current, step, config)}
      end

    catch_all_defs =
      case catch_all do
        :catch_all -> [build_catch_all(current, used_combinators, config)]
        :catch_none -> []
      end

    defs = catch_all_defs ++ Enum.reverse(new_defs) ++ defs
    compile(next_combinators, defs, new_inline ++ inline, next, step, config)
  end

  ## Unbound combinators

  defp compile_unbound_combinator({:traverse, combinators, _, traversal}, current, step, config) do
    compile_unbound_combinator({:traverse, combinators, traversal}, current, step, config)
  end

  defp compile_unbound_combinator({:traverse, combinators, traversal}, current, step, config) do
    arg = quote(do: arg)
    line = quote(do: combinator__line)
    column = quote(do: combinator__column)

    # Define the entry point that gets the current accumulator,
    # put it in the stack and then continues with recursion.
    call_acc = []
    call_stack = quote(generated: true, do: [combinator__acc | combinator__stack])

    {next, step} = build_next(step, config)
    first_body = invoke_next(next, arg, call_acc, call_stack, line, column)
    first_def = build_def(current, arg, [], first_body)

    config = update_in(config.stack_depth, & &1 + 1)
    {defs, inline, last, step} = compile(combinators, [first_def], [], next, step, config)

    # No we need to traverse the accumulator with the user code and
    # concatenate with the previous accumulator at the top of the stack.
    user_acc = traversal.(quote(do: combinator__acc))
    last_acc = quote(generated: true, do: unquote(user_acc) ++ hd(combinator__stack))
    last_stack = quote(generated: true, do: tl(combinator__stack))

    {next, step} = build_next(step, config)
    last_body = invoke_next(next, arg, last_acc, last_stack, line, column)
    last_def = build_def(last, arg, [], last_body)

    inline = [{current, @arity}, {last, @arity} | inline]
    {Enum.reverse([last_def | defs]), inline, next, step, :catch_none}
  end

  defp compile_unbound_combinator({:many, combinators}, current, step, config) do
    {failure, step} = build_next(step, config)
    config = %{config | catch_all: failure, stack_depth: 0}
    {defs, inline, success, step} = compile(combinators, [], [], current, step, config)
    def = build_proxy_to(success, current)
    {Enum.reverse([def | defs]), [{success, @arity} | inline], failure, step, :catch_none}
  end

  ## Bound combinators

  # A bound combinator is a combinator where the number of inputs, guards,
  # outputs, cursor shifts are known at compilation time. We inline those bound
  # combinators into a single bitstring pattern for performance. Currently error
  # reporting will accuse the beginning of the bound combinator in case of errors
  # but such can be addressed if desired.

  defp compile_bound_combinator(inputs, guards, outputs, cursor, current, step, config) do
    arg = {:<<>>, [], inputs ++ [quote(do: rest :: binary)]}
    acc = quote(do: unquote(outputs) ++ combinator__acc)
    {next, step} = build_next(step, config)
    stack = quote(do: combinator__stack)

    body =
      rewrite_cursor(cursor, fn line, column ->
        invoke_next(next, quote(do: rest), acc, stack, line, column)
      end)

    match_def = build_def(current, arg, guards, body)
    {[match_def], [], next, step, :catch_all}
  end

  defp take_bound_combinators(combinators) do
    take_bound_combinators(combinators, [], [], [], [], cursor_pair(), 0)
  end

  defp take_bound_combinators(combinators, inputs, guards, outputs, acc, cursor, counter) do
    with [combinator | combinators] <- combinators,
         {:ok, new_inputs, new_guards, new_outputs, new_cursor, new_counter} <-
           bound_combinator(combinator, cursor, counter) do
      take_bound_combinators(
        combinators,
        inputs ++ new_inputs,
        guards ++ new_guards,
        new_outputs ++ outputs,
        [combinator | acc],
        new_cursor,
        new_counter
      )
    else
      _ ->
        {combinators, inputs, guards, outputs, acc, cursor, counter}
    end
  end

  defp bound_combinator({:literal, binary}, cursor, counter) do
    cursor =
      case String.split(binary, "\n") do
        [single] ->
          add_column(cursor, String.length(single))

        [_ | _] = many ->
          column = many |> List.last() |> String.length()
          add_line(cursor, length(many) - 1, column + 1)
      end

    {:ok, [binary], [], [binary], cursor, counter}
  end

  defp bound_combinator({:bin_segment, inclusive, exclusive, modifiers}, cursor, counter) do
    {var, counter} = build_var(counter)
    input = apply_bin_modifiers(var, modifiers)
    guards = compile_bin_ranges(var, inclusive, exclusive)

    cursor =
      if newline_allowed?(inclusive) and not newline_forbidden?(exclusive) do
        rewrite_cursor(cursor, fn line, column ->
          quote do
            {combinator__line, combinator__column} =
              case unquote(var) do
                ?\n -> {unquote(line) + 1, 1}
                _ -> {unquote(line), unquote(column) + 1}
              end
          end
        end)
      else
        add_column(cursor, 1)
      end

    {:ok, [input], guards, [var], cursor, counter}
  end

  defp bound_combinator({:label, combinators, _labels}, cursor, counter) do
    case take_bound_combinators(combinators, [], [], [], [], cursor, counter) do
      {[], inputs, guards, outputs, _, cursor, counter} ->
        {:ok, inputs, guards, outputs, cursor, counter}

      {_, _, _, _, _, _, _} ->
        :error
    end
  end

  defp bound_combinator({:traverse, combinators, compile_fun, _runtime_fun}, cursor, counter) do
    case take_bound_combinators(combinators, [], [], [], [], cursor, counter) do
      {[], inputs, guards, outputs, _, cursor, counter} ->
        {:ok, inputs, guards, compile_fun.(outputs), cursor, counter}

      {_, _, _, _, _, _, _} ->
        :error
    end
  end

  defp bound_combinator(_, _cursor, _counter) do
    :error
  end

  ## Cursor handling

  defp cursor_pair() do
    quote(do: {combinator__line, combinator__column})
  end

  defp rewrite_cursor({line, column}, fun) do
    fun.(line, column)
  end

  defp rewrite_cursor(cursor, fun) do
    {line, column} = cursor_pair()

    quote do
      unquote(cursor)
      unquote(fun.(line, column))
    end
  end

  defp add_column({line, {:+, _, [column, current]}}, extra)
       when is_integer(current) and is_integer(extra) do
    {line, {:+, [], [column, current + extra]}}
  end

  defp add_column({line, column}, extra) when is_integer(extra) do
    {line, {:+, [], [column, extra]}}
  end

  defp add_column(past, extra) when is_integer(extra) do
    quote do
      unquote(past)
      combinator__column = combinator__column + unquote(extra)
    end
  end

  defp add_line({{:+, _, [line, current]}, _}, extra, column)
       when is_integer(current) and is_integer(extra) do
    {{:+, [], [line, current + extra]}, column}
  end

  defp add_line({line, _}, extra, column) when is_integer(extra) do
    {{:+, [], [line, extra]}, column}
  end

  defp add_line(past, extra, column) when is_integer(extra) do
    quote do
      unquote(past)
      combinator__line = combinator__line + unquote(extra)
      combinator__column = unquote(column)
    end
  end

  defp newline_allowed?([]), do: true

  defp newline_allowed?(ors) do
    Enum.any?(ors, fn
      _.._ = range -> ?\n in range
      codepoint -> ?\n === codepoint
    end)
  end

  defp newline_forbidden?([]), do: false

  defp newline_forbidden?(ands) do
    Enum.any?(ands, fn
      {:not, _.._ = range} -> ?\n in range
      {:not, codepoint} -> ?\n === codepoint
    end)
  end

  ## Label

  defp labels(combinators) do
    Enum.map_join(combinators, ", followed by ", &label/1)
  end

  defp label({:literal, binary}) do
    "literal #{inspect(binary)}"
  end

  defp label({:label, _document, label}) do
    label
  end

  defp label({:bin_segment, inclusive, exclusive, modifiers}) do
    inclusive = Enum.map(inclusive, &inspect_bin_range(&1))
    exclusive = Enum.map(exclusive, &inspect_bin_range(elem(&1, 1)))

    prefix =
      cond do
        :utf8 in modifiers -> "utf8 codepoint"
        :utf16 in modifiers -> "utf16 codepoint"
        :utf32 in modifiers -> "utf32 codepoint"
        true -> "byte"
      end

    prefix <> Enum.join([Enum.join(inclusive, " or") | exclusive], ", and not")
  end

  defp label({:many, combinators}) do
    labels(combinators)
  end

  defp label({:traverse, combinators, _}) do
    labels(combinators)
  end

  defp label({:traverse, combinators, _, _}) do
    labels(combinators)
  end

  ## Bin segments

  defp compile_bin_ranges(var, ors, ands) do
    ands = Enum.map(ands, &bin_range_to_guard(var, &1))

    if ors == [] do
      ands
    else
      ors =
        ors
        |> Enum.map(&bin_range_to_guard(var, &1))
        |> Enum.reduce(&{:or, [], [&2, &1]})

      [ors | ands]
    end
  end

  defp bin_range_to_guard(var, range) do
    case range do
      min..max when min < max ->
        quote(do: unquote(var) >= unquote(min) and unquote(var) <= unquote(max))

      min..max when min > max ->
        quote(do: unquote(var) >= unquote(max) and unquote(var) <= unquote(min))

      min..min ->
        quote(do: unquote(var) === unquote(min))

      min when is_integer(min) ->
        quote(do: unquote(var) === unquote(min))

      {:not, min..max} when min < max ->
        quote(do: unquote(var) < unquote(min) or unquote(var) > unquote(max))

      {:not, min..max} when min > max ->
        quote(do: unquote(var) < unquote(max) or unquote(var) > unquote(min))

      {:not, min..min} ->
        quote(do: unquote(var) !== unquote(min))

      {:not, min} when is_integer(min) ->
        quote(do: unquote(var) !== unquote(min))
    end
  end

  defp inspect_bin_range(min..max) do
    if ascii?(min) and ascii?(max) do
      <<" in the range ", ??, min, ?., ?., ??, max>>
    else
      " in the range #{Integer.to_string(min)}..#{Integer.to_string(max)}"
    end
  end

  defp inspect_bin_range(min) do
    if ascii?(min) do
      <<" equal to ", ??, min>>
    else
      " equal to #{Integer.to_string(min)}"
    end
  end

  defp ascii?(char), do: char >= 32 and char <= 126

  defp apply_bin_modifiers(expr, modifiers) do
    case modifiers do
      [] ->
        expr

      _ ->
        modifiers = Enum.map(modifiers, &Macro.var(&1, __MODULE__))
        {:::, [], [expr, Enum.reduce(modifiers, &{:-, [], [&2, &1]})]}
    end
  end

  ## Helpers

  defp build_var(counter) do
    {{:"x#{counter}", [], __MODULE__}, counter + 1}
  end

  defp build_next(step, %{name: name}) do
    {:"#{name}__#{step}", step + 1}
  end

  defp invoke_next(next, rest, acc, stack, line, column) do
    {next, [], [rest, acc, stack, line, column]}
  end

  defp build_def(name, arg, guards, body) do
    args = quote(do: [combinator__acc, combinator__stack, combinator__line, combinator__column])

    guards =
      case guards do
        [] -> true
        _ -> Enum.reduce(guards, &{:and, [], [&2, &1]})
      end

    {name, [arg | args], guards, body}
  end

  defp build_catch_all(name, combinators, %{catch_all: nil, labels: labels}) do
    reason = error_reason(combinators, labels)
    args = quote(do: [rest, acc, stack, line, column])
    body = quote(do: {:error, unquote(reason), rest, line, column})
    {name, args, true, body}
  end

  defp build_catch_all(name, _combinators, %{catch_all: next, stack_depth: 0}) do
    build_proxy_to(name, next)
  end

  defp build_catch_all(name, _combinators, %{catch_all: next, stack_depth: n}) do
    vars = quote(do: [rest, acc, stack, line, column])
    [rest, acc, stack, line, column] = vars
    args = [rest, quote(do: _), build_stack_depth(n, acc, stack), line, column]
    body = {next, [], vars}
    {name, args, true, body}
  end

  defp build_stack_depth(1, acc, stack), do: [{:|, [], [acc, stack]}]
  defp build_stack_depth(n, acc, stack), do: [quote(do: _) | build_stack_depth(n - 1, acc, stack)]

  defp build_proxy_to(name, next) do
    args = quote(do: [rest, acc, stack, line, column])
    body = {next, [], args}
    {name, args, true, body}
  end

  defp error_reason(combinators, []) do
    "expected " <> labels(combinators)
  end

  defp error_reason(_combinators, [head]) do
    "expected #{head}"
  end

  defp error_reason(_combinators, [head | tail]) do
    "expected #{head} while processing #{Enum.join(tail, " inside ")}"
  end
end
