defmodule NimbleParsec.Compiler do
  @moduledoc false
  @arity 6

  def entry_point(name) do
    doc = """
    Parses the given `binary` as #{name}.

    Returns `{:ok, [token], rest, context, line, byte_offset}` or
    `{:error, reason, rest, context, line, byte_offset}`.

    ## Options

      * `:line` - the initial line, defaults to 1
      * `:byte_offset` - the initial byte offset, defaults to 0
      * `:context` - the initial context value. It will be converted
        to a map
    """

    spec =
      quote do
        unquote(name)(binary, keyword) ::
          {:ok, [term], rest, context, line, byte_offset}
          | {:error, reason, rest, context, line, byte_offset}
        when line: {pos_integer, byte_offset},
             byte_offset: pos_integer,
             rest: binary,
             reason: String.t(),
             context: map()
      end

    args = quote(do: [binary, opts \\ []])
    guards = quote(do: is_binary(binary))

    body =
      quote do
        line = Keyword.get(opts, :line, 1)
        offset = Keyword.get(opts, :byte_offset, 0)
        context = Map.new(Keyword.get(opts, :context, []))

        case unquote(:"#{name}__0")(binary, [], [], context, {line, offset}, offset) do
          {:ok, acc, rest, context, line, offset} ->
            {:ok, :lists.reverse(acc), rest, context, line, offset}

          {:error, _, _, _, _, _} = error ->
            error
        end
      end

    {doc, spec, {name, args, guards, body}}
  end

  def compile_pattern([]) do
    raise ArgumentError, "cannot compile empty parser combinator"
  end

  def compile_pattern(combinators) do
    case take_bound_combinators(Enum.reverse(combinators)) do
      {[], inputs, guards, _, _, _, _, _} -> {inputs, guards_list_to_quoted(guards)}
      _ -> :error
    end
  end

  def compile(name, [], _opts) do
    raise ArgumentError, "cannot compile #{inspect(name)} with an empty parser combinator"
  end

  def compile(name, combinators, opts) when is_list(combinators) do
    debug? = Keyword.get(opts, :debug, false)
    inline? = Keyword.get(opts, :inline, false)
    {defs, inline} = compile(name, combinators)

    if debug? do
      IO.puts(:stderr, NimbleParsec.Printer.format_functions(defs, inline, inline?))
    end

    if inline? do
      {defs, inline}
    else
      {defs, []}
    end
  end

  defp compile(name, combinators) do
    config = %{
      acc_depth: 0,
      catch_all: nil,
      labels: [],
      name: name,
      replace: false
    }

    {next, step} = build_next(0, config)

    {defs, inline, last, _step} =
      combinators
      |> Enum.reverse()
      |> compile([], [], next, step, config)

    {Enum.reverse([build_ok(last) | defs]), [{last, @arity} | inline]}
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
        {[combinator | combinators], [], [], [], [], _, _, _} ->
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

        {combinators, inputs, guards, outputs, acc, line, offset, _} ->
          {combinators, Enum.reverse(acc),
           compile_bound_combinator(inputs, guards, outputs, line, offset, current, step, config)}
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

  defp compile_unbound_combinator({:parsec, parsec}, current, step, config) do
    {next, step} = build_next(step, config)
    head = quote(do: [rest, acc, stack, context, line, offset])

    catch_all =
      case config do
        %{catch_all: nil} ->
          quote(do: error)

        %{catch_all: catch_all, acc_depth: n} ->
          {_, ^head, _, body} = build_proxy_to(current, catch_all, n)
          body
      end

    body =
      quote do
        case unquote(:"#{parsec}__0")(rest, acc, [], context, line, offset) do
          {:ok, acc, rest, context, line, offset} ->
            unquote(next)(rest, acc, stack, context, line, offset)

          {:error, _, _, _, _, _} = error ->
            unquote(catch_all)
        end
      end

    def = {current, head, true, body}
    {[def], [{current, @arity}], next, step, :catch_none}
  end

  defp compile_unbound_combinator(
         {:traverse, combinators, constant?, traversal},
         current,
         step,
         config
       ) do
    fun = &traverse(traversal, &1, &2, &3, &4, &5, &6, config)
    config = if constant?, do: put_in(config.replace, true), else: config
    compile_unbound_traverse(combinators, current, step, config, fun)
  end

  defp compile_unbound_combinator({:times, combinators, 0, count}, current, step, config) do
    if all_bound_combinators?(combinators) do
      compile_bound_times(combinators, count, current, step, config)
    else
      compile_unbound_times(combinators, count, current, step, config)
    end
  end

  defp compile_unbound_combinator({:repeat, combinators, while}, current, step, config) do
    {failure, step} = build_next(step, config)
    config = %{config | catch_all: failure, acc_depth: 0}

    if all_bound_combinators?(combinators) do
      compile_bound_repeat(combinators, while, current, failure, step, config)
    else
      compile_unbound_repeat(combinators, while, current, failure, step, config)
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

  ## Traverse

  defp compile_unbound_traverse([], current, step, config, fun) do
    {next, step} = build_next(step, config)

    head = quote(do: [rest, acc, stack, context, line, offset])
    [rest, _, _, context, line, offset] = head
    body = fun.(next, rest, [], context, line, offset)
    def = {current, head, true, body}
    {[def], [{current, @arity}], next, step, :catch_none}
  end

  defp compile_unbound_traverse(combinators, current, step, config, fun) do
    {next, step} = build_next(step, config)
    head = quote(do: [rest, acc, stack, context, line, offset])
    args = quote(do: [rest, [], [acc | stack], context, line, offset])
    body = {next, [], args}
    first_def = {current, head, true, body}

    config = update_in(config.acc_depth, &(&1 + 1))
    {defs, inline, last, step} = compile(combinators, [first_def], [], next, step, config)

    # Now we need to traverse the accumulator with the user code and
    # concatenate with the previous accumulator at the top of the stack.
    {next, step} = build_next(step, config)
    head = quote(do: [rest, user_acc, [acc | stack], context, line, offset])
    [rest, user_acc, _, context, line, offset] = head
    body = fun.(next, rest, user_acc, context, line, offset)
    last_def = {last, head, true, body}

    inline = [{current, @arity}, {last, @arity} | inline]
    {Enum.reverse([last_def | defs]), inline, next, step, :catch_none}
  end

  defp traverse(_traversal, next, _rest, _user_acc, _context, _line, _offset, %{replace: true}) do
    quote(do: unquote(next)(rest, acc, stack, context, line, offset))
  end

  defp traverse(traversal, next, rest, user_acc, context, line, offset, %{replace: false}) do
    case apply_traverse(traversal, rest, user_acc, context, line, offset) do
      {user_acc, ^context} when user_acc != :error ->
        quote(do: unquote(next)(rest, unquote(user_acc) ++ acc, stack, context, line, offset))

      quoted ->
        quote do
          case unquote(quoted) do
            {user_acc, context} when is_list(user_acc) ->
              unquote(next)(rest, user_acc ++ acc, stack, context, line, offset)

            {:error, reason} ->
              {:error, reason, rest, context, line, offset}
          end
        end
    end
  end

  defp apply_traverse(mfargs, rest, acc, context, line, offset) do
    apply_traverse(Enum.reverse(mfargs), rest, {acc, context}, line, offset)
  end

  defp apply_traverse([mfargs | tail], rest, {acc, context}, line, offset) when acc != :error do
    acc_context = apply_mfa(mfargs, [rest, acc, context, line, offset])
    apply_traverse(tail, rest, acc_context, line, offset)
  end

  defp apply_traverse([], _rest, acc_context, _line, _offset) do
    acc_context
  end

  defp apply_traverse(tail, rest, acc_context, line, offset) do
    pattern = quote(do: {acc, context} when is_list(acc))
    args = [rest, quote(do: acc), quote(do: context), line, offset]

    entries =
      Enum.map(tail, fn mfargs ->
        quote(do: unquote(pattern) <- unquote(apply_mfa(mfargs, args)))
      end)

    quote do
      with unquote(pattern) <- unquote(acc_context), unquote_splicing(entries) do
        {acc, context}
      end
    end
  end

  ## Repeat

  defp compile_bound_repeat(combinators, while, current, failure, step, config) do
    {defs, recur, next, step} =
      case apply_mfa(while, quote(do: [rest, context, line, offset])) do
        {:cont, quote(do: context)} ->
          {[], current, current, step}

        quoted ->
          {next, step} = build_next(step, config)
          head = args = quote(do: [rest, acc, stack, context, line, offset])
          body = repeat_while(quoted, next, args, failure, args)
          {[{current, head, true, body}], current, next, step}
      end

    {defs, inline, success, step} = compile(combinators, defs, [], next, step, config)
    def = build_proxy_to(success, recur, 0)
    {Enum.reverse([def | defs]), [{success, @arity} | inline], failure, step, :catch_none}
  end

  defp compile_unbound_repeat(combinators, while, current, failure, step, config) do
    {recur, step} = build_next(step, config)
    {defs, inline, success, step} = compile(combinators, [], [], recur, step, config)

    {next, step} = build_next(step, config)
    head = quote(do: [_, _, [{rest, acc, context, line, offset} | stack], _, _, _])
    args = quote(do: [rest, acc, stack, context, line, offset])
    body = {next, [], args}
    failure_def = {failure, head, true, body}

    while = apply_mfa(while, quote(do: [rest, context, line, offset]))
    cont = quote(do: {rest, acc, context, line, offset})

    head =
      quote do
        [inner_rest, inner_acc, [unquote(cont) | stack], inner_context, inner_line, inner_offset]
      end

    cont = quote(do: {inner_rest, inner_acc ++ acc, inner_context, inner_line, inner_offset})

    true_args =
      quote do
        [inner_rest, [], [unquote(cont) | stack], inner_context, inner_line, inner_offset]
      end

    false_args = quote(do: [rest, acc, stack, context, line, offset])
    body = repeat_while(while, recur, true_args, next, false_args)
    success_def = {success, head, true, body}

    head = quote(do: [rest, acc, stack, context, line, offset])

    true_args =
      quote do
        [rest, [], [{rest, acc, context, line, offset} | stack], context, line, offset]
      end

    false_args = quote(do: [rest, acc, stack, context, line, offset])
    body = repeat_while(while, recur, true_args, next, false_args)
    current_def = {current, head, true, body}

    defs = [current_def | Enum.reverse([success_def, failure_def | defs])]
    inline = [{current, @arity}, {success, @arity}, {failure, @arity} | inline]
    {defs, inline, next, step, :catch_none}
  end

  defp repeat_while({:cont, quote(do: context)}, true_name, true_args, _false_name, _false_args) do
    {true_name, [], true_args}
  end

  defp repeat_while({:halt, quote(do: context)}, _true_name, _true_args, false_name, false_args) do
    {false_name, [], false_args}
  end

  defp repeat_while(quoted, true_name, true_args, false_name, false_args) do
    quote do
      case unquote(quoted) do
        {:cont, context} -> unquote({true_name, [], true_args})
        {:halt, context} -> unquote({false_name, [], false_args})
      end
    end
  end

  ## Repeat up to

  defp compile_bound_times(combinators, count, current, step, config) do
    {failure, step} = build_next(step, config)
    {recur, step} = build_next(step, config)

    head = quote(do: [rest, acc, stack, context, line, offset])
    args = quote(do: [rest, acc, [unquote(count) | stack], context, line, offset])
    body = {recur, [], args}
    current_def = {current, head, true, body}

    config = %{config | catch_all: failure, acc_depth: 0}
    {defs, inline, success, step} = compile(combinators, [current_def], [], recur, step, config)

    {next, step} = build_next(step, config)
    head = quote(do: [rest, acc, [1 | stack], context, line, offset])
    args = quote(do: [rest, acc, stack, context, line, offset])
    body = {next, [], args}
    success_def0 = {success, head, true, body}

    head = quote(do: [rest, acc, [count | stack], context, line, offset])
    args = quote(do: [rest, acc, [count - 1 | stack], context, line, offset])
    body = {recur, [], args}
    success_def1 = {success, head, true, body}

    head = quote(do: [rest, acc, [_ | stack], context, line, offset])
    args = quote(do: [rest, acc, stack, context, line, offset])
    body = {next, [], args}
    failure_def = {failure, head, true, body}

    defs = Enum.reverse([success_def1, success_def0, failure_def | defs])
    inline = [{current, @arity}, {success, @arity}, {failure, @arity} | inline]
    {defs, inline, next, step, :catch_none}
  end

  defp compile_unbound_times(combinators, count, current, step, config) do
    {failure, step} = build_next(step, config)
    {recur, step} = build_next(step, config)

    head = quote(do: [rest, acc, stack, context, line, offset])
    cont = quote(do: {unquote(count), rest, acc, context, line, offset})
    args = quote(do: [rest, [], [unquote(cont) | stack], context, line, offset])
    body = {recur, [], args}
    current_def = {current, head, true, body}

    config = %{config | catch_all: failure, acc_depth: 0}
    {defs, inline, success, step} = compile(combinators, [current_def], [], recur, step, config)

    {next, step} = build_next(step, config)
    head = quote(do: [rest, user_acc, [{1, _, acc, _, _, _} | stack], context, line, offset])
    args = quote(do: [rest, user_acc ++ acc, stack, context, line, offset])
    body = {next, [], args}
    success_def0 = {success, head, true, body}

    head = quote(do: [rest, user_acc, [{count, _, acc, _, _, _} | stack], context, line, offset])
    cont = quote(do: {count - 1, rest, user_acc ++ acc, context, line, offset})
    args = quote(do: [rest, [], [unquote(cont) | stack], context, line, offset])
    body = {recur, [], args}
    success_def1 = {success, head, true, body}

    head = quote(do: [_, _, [{_, rest, acc, context, line, offset} | stack], _, _, _])
    args = quote(do: [rest, acc, stack, context, line, offset])
    body = {next, [], args}
    failure_def = {failure, head, true, body}

    defs = Enum.reverse([success_def1, success_def0, failure_def | defs])
    inline = [{current, @arity}, {success, @arity}, {failure, @arity} | inline]
    {defs, inline, next, step, :catch_none}
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
    config = update_in(config.acc_depth, &(&1 + 2))

    {first, defs, inline, step} =
      compile_unbound_choice(Enum.reverse(choices), [], [], :unused, step, done, config)

    head = quote(do: [rest, acc, stack, context, line, offset])
    cont = quote(do: {rest, context, line, offset})
    args = quote(do: [rest, [], [unquote(cont), acc | stack], context, line, offset])
    body = {first, [], args}
    def = {current, head, true, body}

    {[def | Enum.reverse(defs)], [{current, @arity} | inline], done, step, :catch_none}
  end

  defp compile_unbound_choice([], defs, inline, previous, step, _success, _config) do
    # Discard the last failure definition that won't be used.
    {previous, tl(defs), tl(inline), step - 1}
  end

  defp compile_unbound_choice([choice | choices], defs, inline, _previous, step, done, config) do
    {current, step} = build_next(step, config)
    {defs, inline, success, step} = compile(choice, defs, inline, current, step, config)

    head = quote(do: [rest, acc, [_, previous_acc | stack], context, line, offset])
    args = quote(do: [rest, acc ++ previous_acc, stack, context, line, offset])
    body = {done, [], args}
    success_def = {success, head, true, body}

    {failure, step} = build_next(step, config)
    head = quote(do: [_, _, [{rest, context, line, offset} | _] = stack, _, _, _])
    args = quote(do: [rest, [], stack, context, line, offset])
    body = {current, [], args}
    failure_def = {failure, head, true, body}

    defs = [failure_def, success_def | defs]
    inline = [{failure, @arity}, {success, @arity} | inline]
    config = %{config | catch_all: failure, acc_depth: 0}
    compile_unbound_choice(choices, defs, inline, current, step, done, config)
  end

  ## Bound combinators

  # A bound combinator is a combinator where the number of inputs, guards,
  # outputs, line and offset shifts are known at compilation time. We inline
  # those bound combinators into a single bitstring pattern for performance.
  # Currently error reporting will accuse the beginning of the bound combinator
  # in case of errors but such can be addressed if desired.

  defp compile_bound_combinator(inputs, guards, outputs, line, offset, current, step, config) do
    {next, step} = build_next(step, config)
    bin = {:<<>>, [], inputs ++ [quote(do: rest :: binary)]}
    acc = if config.replace, do: quote(do: acc), else: quote(do: unquote(outputs) ++ acc)

    head = quote(do: [unquote(bin), acc, stack, context, comb__line, comb__offset])
    args = quote(do: [rest, unquote(acc), stack, context, unquote(line), unquote(offset)])
    body = {next, [], args}

    guards = guards_list_to_quoted(guards)
    def = {current, head, guards, body}
    {[def], [], next, step, :catch_all}
  end

  defp all_bound_combinators?(combinators) do
    {line, offset} = line_offset_pair()

    Enum.all?(combinators, fn combinator ->
      case bound_combinator(combinator, line, offset, 0) do
        {:ok, _, _, _, _, _, _} -> true
        :error -> false
      end
    end)
  end

  defp take_bound_combinators(combinators) do
    {line, offset} = line_offset_pair()
    take_bound_combinators(combinators, [], [], [], [], line, offset, 0)
  end

  defp take_bound_combinators(combinators, inputs, guards, outputs, acc, line, offset, counter) do
    with [combinator | combinators] <- combinators,
         {:ok, new_inputs, new_guards, new_outputs, new_line, new_offset, new_counter} <-
           bound_combinator(combinator, line, offset, counter) do
      take_bound_combinators(
        combinators,
        inputs ++ new_inputs,
        guards ++ new_guards,
        merge_output(new_outputs, outputs),
        [combinator | acc],
        new_line,
        new_offset,
        new_counter
      )
    else
      _ ->
        {combinators, inputs, guards, outputs, acc, line, offset, counter}
    end
  end

  defp merge_output(left, right) when is_list(left) and is_list(right), do: left ++ right
  defp merge_output(left, right), do: quote(do: unquote(left) ++ unquote(right))

  defp bound_combinator({:string, string}, line, offset, counter) do
    size = byte_size(string)

    line =
      case String.split(string, "\n") do
        [_] ->
          line

        [_ | _] = many ->
          last_size = many |> List.last() |> byte_size()
          line_offset = add_offset(offset, size - last_size)

          quote do
            {elem(unquote(line), 0) + unquote(length(many) - 1), unquote(line_offset)}
          end
      end

    offset = add_offset(offset, size)
    {:ok, [string], [], [string], line, offset, counter}
  end

  defp bound_combinator({:bin_segment, inclusive, exclusive, modifiers}, line, offset, counter) do
    {var, counter} = build_var(counter)
    input = apply_bin_modifiers(var, modifiers)
    guards = compile_bin_ranges(var, inclusive, exclusive)

    offset =
      if :integer in modifiers do
        add_offset(offset, 1)
      else
        add_offset(offset, quote(do: byte_size(<<unquote(input)>>)))
      end

    line =
      if newline_allowed?(inclusive) and not newline_forbidden?(exclusive) do
        quote do
          case unquote(var) do
            ?\n -> {elem(unquote(line), 0) + 1, unquote(offset)}
            _ -> unquote(line)
          end
        end
      else
        line
      end

    {:ok, [input], guards, [var], line, offset, counter}
  end

  defp bound_combinator({:label, combinators, _labels}, line, offset, counter) do
    case take_bound_combinators(combinators, [], [], [], [], line, offset, counter) do
      {[], inputs, guards, outputs, _, line, offset, counter} ->
        {:ok, inputs, guards, outputs, line, offset, counter}

      {_, _, _, _, _, _, _, _} ->
        :error
    end
  end

  defp bound_combinator({:traverse, combinators, _constant?, mfargs}, line, offset, counter) do
    case take_bound_combinators(combinators, [], [], [], [], line, offset, counter) do
      {[], inputs, guards, outputs, _, line, offset, counter} ->
        {rest, context} = quote(do: {rest, context})

        case apply_traverse(mfargs, rest, outputs, context, line, offset) do
          {outputs, ^context} when outputs != :error ->
            {:ok, inputs, guards, outputs, line, offset, counter}

          _ ->
            :error
        end

      {_, _, _, _, _, _, _, _} ->
        :error
    end
  end

  defp bound_combinator(_, _line, _offset, _counter) do
    :error
  end

  ## Line and offset handling

  defp line_offset_pair() do
    quote(do: {comb__line, comb__offset})
  end

  defp add_offset({:+, _, [var, current]}, extra)
       when is_integer(current) and is_integer(extra) do
    {:+, [], [var, current + extra]}
  end

  defp add_offset(var, extra) do
    {:+, [], [var, extra]}
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

  defp label({:string, binary}) do
    "string #{inspect(binary)}"
  end

  defp label({:label, _combinator, label}) do
    label
  end

  defp label({:bin_segment, inclusive, exclusive, modifiers}) do
    inclusive = Enum.map(inclusive, &inspect_bin_range(&1))
    exclusive = Enum.map(exclusive, &inspect_bin_range(elem(&1, 1)))

    prefix =
      cond do
        :integer in modifiers -> "byte"
        :utf8 in modifiers -> "utf8 codepoint"
        :utf16 in modifiers -> "utf16 codepoint"
        :utf32 in modifiers -> "utf32 codepoint"
      end

    prefix <> Enum.join([Enum.join(inclusive, " or") | exclusive], ", and not")
  end

  defp label({:repeat, combinators, _}) do
    labels(combinators)
  end

  defp label({:times, combinators, _, _}) do
    labels(combinators)
  end

  defp label({:choice, choices}) do
    Enum.map_join(choices, " or ", &labels/1)
  end

  defp label({:traverse, combinators, _, _}) do
    labels(combinators)
  end

  defp label({:parsec, name}) do
    Atom.to_string(name)
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

  defp apply_mfa({mod, fun, args}, extra) do
    apply(mod, fun, extra ++ args)
  end

  defp guards_list_to_quoted([]), do: true
  defp guards_list_to_quoted(guards), do: Enum.reduce(guards, &{:and, [], [&2, &1]})

  defp build_var(counter) do
    {{:"x#{counter}", [], __MODULE__}, counter + 1}
  end

  defp build_next(step, %{name: name}) do
    {:"#{name}__#{step}", step + 1}
  end

  defp build_ok(current) do
    head = quote(do: [rest, acc, _stack, context, line, offset])
    body = quote(do: {:ok, acc, rest, context, line, offset})
    {current, head, true, body}
  end

  defp build_catch_all(name, combinators, %{catch_all: nil, labels: labels}) do
    reason = error_reason(combinators, labels)
    args = quote(do: [rest, _acc, _stack, context, line, offset])
    body = quote(do: {:error, unquote(reason), rest, context, line, offset})
    {name, args, true, body}
  end

  defp build_catch_all(name, _combinators, %{catch_all: next, acc_depth: n}) do
    build_proxy_to(name, next, n)
  end

  defp build_acc_depth(1, acc, stack), do: [{:|, [], [acc, stack]}]
  defp build_acc_depth(n, acc, stack), do: [quote(do: _) | build_acc_depth(n - 1, acc, stack)]

  defp build_proxy_to(name, next, 0) do
    args = quote(do: [rest, acc, stack, context, line, offset])
    body = {next, [], args}
    {name, args, true, body}
  end

  defp build_proxy_to(name, next, n) do
    args = quote(do: [rest, acc, stack, context, line, offset])
    [_, acc, stack, _, _, _] = args

    body =
      quote do
        unquote(build_acc_depth(n, acc, stack)) = stack
        unquote(next)(rest, acc, stack, context, line, offset)
      end

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
