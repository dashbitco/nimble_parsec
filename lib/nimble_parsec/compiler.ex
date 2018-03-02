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
    head = quote(do: [rest, acc, _stack, line, column])
    body = quote(do: {:ok, :lists.reverse(acc), rest, line, column})
    {current, head, true, body}
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

  defp compile_unbound_combinator({:traverse, [], traversal}, current, step, config) do
    {next, step} = build_next(step, config)
    user_acc = traversal.([])
    head = quote(do: [arg, acc, stack, line, column])
    args = quote(do: [arg, unquote(user_acc) ++ acc, stack, line, column])
    body = {next, [], args}
    def = {current, head, true, body}
    {[def], [{current, @arity}], next, step, :catch_none}
  end

  defp compile_unbound_combinator({:traverse, combinators, traversal}, current, step, config) do
    {next, step} = build_next(step, config)
    head = quote(do: [arg, acc, stack, line, column])
    args = quote(do: [arg, [], [acc | stack], line, column])
    body = {next, [], args}
    first_def = {current, head, true, body}

    config = update_in(config.stack_depth, &(&1 + 1))
    {defs, inline, last, step} = compile(combinators, [first_def], [], next, step, config)

    # No we need to traverse the accumulator with the user code and
    # concatenate with the previous accumulator at the top of the stack.
    {next, step} = build_next(step, config)
    user_acc = traversal.(quote(do: user_acc))
    head = quote(do: [arg, user_acc, [acc | stack], line, column])
    args = quote(do: [arg, unquote(user_acc) ++ acc, stack, line, column])
    body = {next, [], args}
    last_def = {last, head, true, body}

    inline = [{current, @arity}, {last, @arity} | inline]
    {Enum.reverse([last_def | defs]), inline, next, step, :catch_none}
  end

  defp compile_unbound_combinator({:repeat, combinators}, current, step, config) do
    if all_bound_combinators?(combinators) do
      compile_bound_repeat(combinators, current, step, config)
    else
      compile_unbound_repeat(combinators, current, step, config)
    end
  end

  defp compile_unbound_combinator({:choice, choices} = combinator, current, step, config) do
    config = update_in(config.labels, &[label(combinator) | &1])

    if Enum.all?(choices, &all_bound_combinators?/1) do
      compile_bound_choice(choices, current, step, config)
    else
      compile_unbound_choice(choices, current, step, config)
    end
  end

  ## Repeat

  defp compile_bound_repeat(combinators, current, step, config) do
    {failure, step} = build_next(step, config)
    config = %{config | catch_all: failure, stack_depth: 0}
    {defs, inline, success, step} = compile(combinators, [], [], current, step, config)
    def = build_proxy_to(success, current, 0)
    {Enum.reverse([def | defs]), [{success, @arity} | inline], failure, step, :catch_none}
  end

  defp compile_unbound_repeat(combinators, current, step, config) do
    {failure, step} = build_next(step, config)
    {next, step} = build_next(step, config)

    head = quote(do: [rest, acc, stack, line, column])
    args = quote(do: [rest, [], [{rest, acc, line, column} | stack], line, column])
    body = {next, [], args}
    current_def = {current, head, true, body}

    config = %{config | catch_all: failure, stack_depth: 0}
    {defs, inline, success, step} = compile(combinators, [current_def], [], next, step, config)

    head = quote(do: [rest, user_acc, [{_, acc, _, _} | stack], line, column])
    args = quote(do: [rest, [], [{rest, user_acc ++ acc, line, column} | stack], line, column])
    body = {next, [], args}
    success_def = {success, head, true, body}

    {next, step} = build_next(step, config)
    head = quote(do: [_, _, [{rest, acc, line, column} | stack], _, _])
    args = quote(do: [rest, acc, stack, line, column])
    body = {next, [], args}
    failure_def = {failure, head, true, body}

    inline = [{current, @arity}, {success, @arity}, {failure, @arity} | inline]
    {Enum.reverse([success_def, failure_def | defs]), inline, next, step, :catch_none}
  end

  ## Choice

  defp compile_bound_choice(choices, current, step, config) do
    {next_name, next_step} = build_next(step, config)

    defs =
      for choice <- choices do
        if choice == [] do
          build_proxy_to(current, next_name, 0)
        else
          {[_, def], [], ^next_name, ^next_step} = compile(choice, [], [], current, step, config)
          def
        end
      end

    catch_all = if [] in choices, do: :catch_none, else: :catch_all
    {defs, [], next_name, next_step, catch_all}
  end

  defp compile_unbound_choice(choices, current, step, config) do
    {done, step} = build_next(step, config)

    # We process choices in reverse order. The last order does not
    # have any fallback besides the requirement to drop the stack
    # this allows us to compose with repeat and traverse.
    config = update_in(config.stack_depth, &(&1 + 2))

    {first, defs, inline, step} =
      compile_unbound_choice(Enum.reverse(choices), [], [], :unused, step, done, config)

    head = quote(do: [rest, acc, stack, line, column])
    args = quote(do: [rest, [], [{rest, line, column}, acc | stack], line, column])
    body = {first, [], args}
    def = {current, head, true, body}

    {Enum.reverse([def | defs]), [{current, @arity} | inline], done, step, :catch_none}
  end

  defp compile_unbound_choice([], defs, inline, previous, step, _success, _config) do
    # Discard the last failure definition that won't be used.
    {previous, tl(defs), tl(inline), step - 1}
  end

  defp compile_unbound_choice([choice | choices], defs, inline, _previous, step, done, config) do
    {current, step} = build_next(step, config)
    {defs, inline, success, step} = compile(choice, defs, inline, current, step, config)

    head = quote(do: [rest, acc, [_, previous_acc | stack], line, column])
    args = quote(do: [rest, acc ++ previous_acc, stack, line, column])
    body = {done, [], args}
    success_def = {success, head, true, body}

    {failure, step} = build_next(step, config)
    head = quote(do: [_, _, [{rest, line, column} | _] = stack, _, _])
    args = quote(do: [rest, [], stack, line, column])
    body = {current, [], args}
    failure_def = {failure, head, true, body}

    defs = [failure_def, success_def | defs]
    inline = [{failure, @arity}, {success, @arity} | inline]
    config = %{config | catch_all: failure, stack_depth: 0}
    compile_unbound_choice(choices, defs, inline, current, step, done, config)
  end

  ## Bound combinators

  # A bound combinator is a combinator where the number of inputs, guards,
  # outputs, cursor shifts are known at compilation time. We inline those bound
  # combinators into a single bitstring pattern for performance. Currently error
  # reporting will accuse the beginning of the bound combinator in case of errors
  # but such can be addressed if desired.

  defp compile_bound_combinator(inputs, guards, outputs, cursor, current, step, config) do
    {next, step} = build_next(step, config)
    pattern = {:<<>>, [], inputs ++ [quote(do: rest :: binary)]}
    head = quote(do: [unquote(pattern), acc, stack, combinator__line, combinator__column])

    body =
      rewrite_cursor(cursor, fn line, column ->
        args = quote(do: [rest, unquote(outputs) ++ acc, stack, unquote(line), unquote(column)])
        {next, [], args}
      end)

    guards =
      case guards do
        [] -> true
        _ -> Enum.reduce(guards, &{:and, [], [&2, &1]})
      end

    def = {current, head, guards, body}
    {[def], [], next, step, :catch_all}
  end

  defp all_bound_combinators?(combinators) do
    cursor = cursor_pair()

    Enum.all?(combinators, fn combinator ->
      case bound_combinator(combinator, cursor, 0) do
        {:ok, _, _, _, _, _} -> true
        :error -> false
      end
    end)
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
        merge_output(new_outputs, outputs),
        [combinator | acc],
        new_cursor,
        new_counter
      )
    else
      _ ->
        {combinators, inputs, guards, outputs, acc, cursor, counter}
    end
  end

  defp merge_output(left, right) when is_list(left) and is_list(right), do: left ++ right
  defp merge_output(left, right), do: quote(do: unquote(left) ++ unquote(right))

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

  defp bound_combinator({:traverse, combinators, fun}, cursor, counter) do
    case take_bound_combinators(combinators, [], [], [], [], cursor, counter) do
      {[], inputs, guards, outputs, _, cursor, counter} ->
        {:ok, inputs, guards, fun.(outputs), cursor, counter}

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

  defp labels([]) do
    "nothing"
  end

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

  defp label({:repeat, combinators}) do
    labels(combinators)
  end

  defp label({:repeat_up_to, combinators, _}) do
    labels(combinators)
  end

  defp label({:choice, choices}) do
    "one of " <> Enum.map_join(choices, ", ", &labels/1)
  end

  defp label({:traverse, combinators, _}) do
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

  defp build_catch_all(name, combinators, %{catch_all: nil, labels: labels}) do
    reason = error_reason(combinators, labels)
    args = quote(do: [rest, acc, stack, line, column])
    body = quote(do: {:error, unquote(reason), rest, line, column})
    {name, args, true, body}
  end

  defp build_catch_all(name, _combinators, %{catch_all: next, stack_depth: n}) do
    build_proxy_to(name, next, n)
  end

  defp build_stack_depth(1, acc, stack), do: [{:|, [], [acc, stack]}]
  defp build_stack_depth(n, acc, stack), do: [quote(do: _) | build_stack_depth(n - 1, acc, stack)]

  defp build_proxy_to(name, next, 0) do
    args = quote(do: [rest, acc, stack, line, column])
    body = {next, [], args}
    {name, args, true, body}
  end

  defp build_proxy_to(name, next, n) do
    vars = quote(do: [rest, acc, stack, line, column])
    [rest, acc, stack, line, column] = vars
    args = [rest, quote(do: _), build_stack_depth(n, acc, stack), line, column]
    body = {next, [], vars}
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
