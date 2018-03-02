# TODO: runtime_composition() and private parsecs
# TODO: Benchmark integer(opts)
# TODO: repeat_until()
# TODO: Docs

defmodule NimbleParsec do
  defmacrop is_combinator(combinator) do
    quote do
      is_list(unquote(combinator))
    end
  end

  @doc """
  Defines a public parser `combinator` with the given `name` and `opts`.

  ## Options

    * `:inline` - when true, inlines clauses that work as redirection for
      other clauses. It is disabled by default because of a bug in Elixir
      v1.5 and v1.6 where unused functions that are inlined cause a
      compilation error

    * `:debug` - when true, writes generated clauses to stderr for debugging

  """
  defmacro defparsec(name, combinator, opts \\ []) do
    quote bind_quoted: [name: name, combinator: combinator, opts: opts] do
      def unquote(name)(binary, opts \\ []) when is_binary(binary) do
        unquote(:"#{name}__0")(binary, [], [], 1, 1)
      end

      {defs, inline} = NimbleParsec.Compiler.compile(name, combinator, opts)

      if Keyword.get(opts, :inline, false) do
        @compile {:inline, inline}
      end

      debug? = Keyword.get(opts, :debug, false)

      for {name, args, guards, body} <- defs do
        defp unquote(name)(unquote_splicing(args)) when unquote(guards), do: unquote(body)

        if debug? do
          IO.puts(:stderr, """
          defp #{Macro.to_string(quote(do: unquote(name)(unquote_splicing(args))))}
               when #{Macro.to_string(guards)} do
            #{Macro.to_string(body)}
          end
          """)
        end
      end

      :ok
    end
  end

  @type t :: [combinator]
  @type bin_modifiers :: :utf8 | :utf16 | :utf32
  @type range :: inclusive_range | exclusive_range
  @type inclusive_range :: Range.t() | char()
  @type exclusive_range :: {:not, Range.t()} | {:not, char()}
  @type min_and_max :: {:min, pos_integer()} | {:max, pos_integer()}
  @type call ::
          {module :: atom(), function :: atom(), args :: [term]}
          | {function :: atom(), args :: [term]}

  # Steps to add a new bound combinator:
  #
  #   1. Update the combinator type
  #   2. Update the compiler bound combinator step
  #   3. Update the compiler label step
  #
  @typep combinator :: bound_combinator | maybe_bound_combinator | unbound_combinator

  @typep bound_combinator ::
           {:bin_segment, [inclusive_range], [exclusive_range], [bin_modifiers]}
           | {:literal, binary}

  @typep maybe_bound_combinator ::
           {:label, t, binary}
           | {:traverse, t, (Macro.t() -> Macro.t())}

  @typep unbound_combinator ::
           {:choice, [t]}
           | {:repeat, t}
           | {:repeat_up_to, t, pos_integer}

  @doc ~S"""
  Returns an empty combinator.

  An empty combinator cannot be compiled on its own.
  """
  def empty() do
    []
  end

  @doc ~S"""
  Defines a single ascii codepoint in the given ranges.

  `ranges` is a list containing one of:

    * a `min..max` range expressing supported codepoints
    * a `codepoint` integer expressing a supported codepoint
    * `{:not, min..max}` expressing not supported codepoints
    * `{:not, codepoint}` expressing a not supported codepoint

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :digit_and_lowercase,
                  empty()
                  |> ascii_char([?0..?9])
                  |> ascii_char([?a..?z])
      end

      MyParser.digit_and_lowercase("1a")
      #=> {:ok, [?1, ?a], "", 1, 3}

      MyParser.digit_and_lowercase("a1")
      #=> {:error, "expected a byte in the range ?0..?9, followed by a byte in the range ?a..?z", "a1", 1, 1}

  """
  @spec ascii_char(t, [range]) :: t
  def ascii_char(combinator \\ empty(), ranges)
      when is_combinator(combinator) and is_list(ranges) do
    {inclusive, exclusive} = split_ranges!(ranges, "ascii_char")
    bin_segment(combinator, inclusive, exclusive, [])
  end

  @doc ~S"""
  Defines a single utf8 codepoint in the given ranges.

  `ranges` is a list containing one of:

    * a `min..max` range expressing supported codepoints
    * a `codepoint` integer expressing a supported codepoint
    * `{:not, min..max}` expressing not supported codepoints
    * `{:not, codepoint}` expressing a not supported codepoint

  Note: currently columns only count codepoints and not graphemes.
  This means the column count will be off when the input contains
  grapheme clusters.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :digit_and_utf8,
                  empty()
                  |> utf8_char([?0..?9])
                  |> utf8_char([])
      end

      MyParser.digit_and_utf8("1é")
      #=> {:ok, [?1, ?é], "", 1, 3}

      MyParser.digit_and_utf8("a1")
      #=> {:error, "expected a utf8 codepoint in the range ?0..?9, followed by a utf8 codepoint", "a1", 1, 1}

  """
  @spec utf8_char(t, [range]) :: t
  def utf8_char(combinator \\ empty(), ranges)
      when is_combinator(combinator) and is_list(ranges) do
    {inclusive, exclusive} = split_ranges!(ranges, "utf8_char")
    bin_segment(combinator, inclusive, exclusive, [:utf8])
  end

  @doc ~S"""
  Adds a label to the combinator to be used in error reports.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :digit_and_lowercase,
                  empty()
                  |> ascii_char([?0..?9])
                  |> ascii_char([?a..?z])
                  |> label("digit followed by lowercase letter")
      end

      MyParser.digit_and_lowercase("1a")
      #=> {:ok, [?1, ?a], "", 1, 3}

      MyParser.digit_and_lowercase("a1")
      #=> {:error, "expected a digit followed by lowercase letter", "a1", 1, 1}

  """
  def label(combinator \\ empty(), to_label, label)
      when is_combinator(combinator) and is_combinator(to_label) and is_binary(label) do
    to_label = reverse_combinators!(to_label, "label")
    [{:label, to_label, label} | combinator]
  end

  @doc ~S"""
  Defines an integer combinator with of exact length or `min` and `max`
  length.

  If you want an integer of unknown size, use `integer(min: 1)`.

  ## Examples

  With exact length:

      defmodule MyParser do
        import NimbleParsec

        defparsec :two_digits_integer, integer(2)
      end

      MyParser.two_digits_integer("123")
      #=> {:ok, [12], "3", 1, 3}

      MyParser.two_digits_integer("1a3")
      #=> {:error, "expected a two digits integer", "1a3", 1, 1}

  With min and max:

      defmodule MyParser do
        import NimbleParsec

        defparsec :two_digits_integer, integer(min: 2, max: 4)
      end

      MyParser.two_digits_integer("123")
      #=> {:ok, [12], "3", 1, 3}

      MyParser.two_digits_integer("1a3")
      #=> {:error, "expected a two digits integer", "1a3", 1, 1}

  """
  @spec integer(t, pos_integer | [min_and_max]) :: t
  def integer(combinator \\ empty(), count)

  def integer(combinator, count)
      when is_combinator(combinator) and is_integer(count) and count > 0 do
    integer = duplicate(ascii_char([?0..?9]), count)
    quoted_traverse(combinator, integer, &quoted_ascii_to_integer/1)
  end

  def integer(combinator, opts) when is_combinator(combinator) and is_list(opts) do
    {min, max} = validate_min_and_max!(opts)
    to_repeat = ascii_char([?0..?9])

    integer =
      if min do
        integer(min)
      else
        empty()
      end

    integer =
      if max do
        times(integer, to_repeat, max: max - (min || 0))
      else
        repeat(integer, to_repeat)
      end

    quoted_traverse(combinator, integer, fn x ->
      quote do
        [head | tail] = :lists.reverse(unquote(x))
        [:lists.foldl(fn x, acc -> x - ?0 + acc * 10 end, head, tail)]
      end
    end)
  end

  defp quoted_ascii_to_integer(vars) when is_list(vars) do
    vars
    |> quoted_ascii_to_integer(1)
    |> Enum.reduce(&{:+, [], [&2, &1]})
    |> List.wrap()
  end

  defp quoted_ascii_to_integer([var | vars], index) do
    [quote(do: (unquote(var) - ?0) * unquote(index)) | quoted_ascii_to_integer(vars, index * 10)]
  end

  defp quoted_ascii_to_integer([], _index) do
    []
  end

  @doc ~S"""
  Concatenates two combinators.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :digit_upper_lower_plus,
                  concat(
                    concat(ascii_char([?0..?9]), ascii_char([?A..?Z])),
                    concat(ascii_char([?a..?z]), ascii_char([?+..?+]))
                  )
      end

      MyParser.digit_upper_lower_plus("1Az+")
      #=> {:ok, [?1, ?A, ?z, ?+], "", 1, 5}

  """
  @spec concat(t, t) :: t
  def concat(left, right) when is_combinator(left) and is_combinator(right) do
    right ++ left
  end

  @doc """
  Duplicates the combinator `to_duplicate` `n` times.
  """
  @spec duplicate(t, t, pos_integer) :: t
  def duplicate(combinator \\ empty(), to_duplicate, n)
      when is_combinator(combinator) and is_combinator(to_duplicate) and is_integer(n) and n >= 1 do
    Enum.reduce(1..n, combinator, fn _, acc -> to_duplicate ++ acc end)
  end

  @doc ~S"""
  Traverses the combinator results with the remote or local function `call`.

  `call` is either a `{module, function, args}` representing
  a remote call or `{function, args}` representing a local call.

  The parser results to be traversed will be prepended to the
  given `args`. The `args` will be injected at the compile site
  and therefore must be escapable via `Macro.escape/1`.

  Notice the results are received in reverse order and
  must be returned in reverse order.

  The number of elements returned does not need to be
  the same as the number of elements given.

  This is a low-level function for changing the parsed result.
  On top of this function, other functions are built, such as
  `map/3` if you want to map over each individual element and
  not worry about ordering, `reduce/3` to reduce all elements
  into a single one, `replace/3` if you want to replace the
  parsed result by a single value and `ignore/3` if you want to
  ignore the parsed result.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :letters_to_chars,
                  ascii_char([?a..?z])
                  |> ascii_char([?a..?z])
                  |> ascii_char([?a..?z])
                  |> traverse({:join_and_wrap, ["-"]})

        defp join_and_wrap(args, joiner) do
          args |> Enum.join(joiner) |> List.wrap()
        end
      end

      MyParser.letters_to_chars("abc")
      #=> {:ok, ["99-98-97"], "", 1, 4}
  """
  @spec traverse(t, t, call) :: t
  def traverse(combinator \\ empty(), to_traverse, call)
      when is_combinator(combinator) and is_combinator(to_traverse) and is_tuple(call) do
    compile_call!(:ok, call, "traverse")
    quoted_traverse(combinator, to_traverse, &compile_call!(&1, call, "traverse"))
  end

  @doc """
  Traverses the quoted `to_traverse` combinator results at
  compile time with `fun`.

  `fun` is an anonymous function that receives an AST and
  returns the traversed AST.

  This is a low-level function that is invoked at compile time
  and is useful in combinators to avoid injecting runtime
  dependencies.
  """
  def quoted_traverse(combinator, to_traverse, fun)
      when is_combinator(combinator) and is_combinator(to_traverse) and is_function(fun, 1) do
    [{:traverse, Enum.reverse(to_traverse), fun} | combinator]
  end

  @doc ~S"""
  Maps over the combinator results with the remote or local function in `call`.

  `call` is either a `{module, function, args}` representing
  a remote call or `{function, args}` representing a local call.

  Each parser result will be invoked individually for the `call`.
  Each result  be prepended to the given `args`. The `args` will
  be injected at the compile site and therefore must be escapable
  via `Macro.escape/1`.

  See `traverse/3` for a low level version of this function.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :letters_to_string_chars,
                  ascii_char([?a..?z])
                  |> ascii_char([?a..?z])
                  |> ascii_char([?a..?z])
                  |> map({Integer, :to_string, []})
      end

      MyParser.letters_to_string_chars("abc")
      #=> {:ok, ["97", "98", "99"], "", 1, 4}
  """
  @spec map(t, t, call) :: t
  def map(combinator \\ empty(), to_map, call)
      when is_combinator(combinator) and is_combinator(to_map) and is_tuple(call) do
    var = Macro.var(:var, __MODULE__)
    call = compile_call!(var, call, "map")

    quoted_traverse(combinator, to_map, fn arg ->
      quote do
        Enum.map(unquote(arg), fn unquote(var) -> unquote(call) end)
      end
    end)
  end

  @doc ~S"""
  Reduces over the combinator results with the remote or local function in `call`.

  `call` is either a `{module, function, args}` representing
  a remote call or `{function, args}` representing a local call.

  The parser results to be reduced will be prepended to the
  given `args`. The `args` will be injected at the compile site
  and therefore must be escapable via `Macro.escape/1`.

  See `traverse/3` for a low level version of this function.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :letters_to_reduced_chars,
                  ascii_char([?a..?z])
                  |> ascii_char([?a..?z])
                  |> ascii_char([?a..?z])
                  |> reduce({Enum, :join, ["-"]})
      end

      MyParser.letters_to_reduced_chars("abc")
      #=> {:ok, ["97-98-99"], "", 1, 4}
  """
  @spec reduce(t, t, call) :: t
  def reduce(combinator \\ empty(), to_reduce, call)
      when is_combinator(combinator) and is_combinator(to_reduce) and is_tuple(call) do
    compile_call!(:ok, call, "reduce")

    quoted_traverse(combinator, to_reduce, fn arg ->
      [compile_call!(quote(do: :lists.reverse(unquote(arg))), call, "reduce")]
    end)
  end

  @doc ~S"""
  Defines a literal binary value.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :literal_t, literal("T")
      end

      MyParser.literal_t("T")
      #=> {:ok, ["T"], "", 1, 2}

      MyParser.literal_t("not T")
      #=> {:error, "expected a literal \"T\"", "not T", 1, 1}

  """
  @spec literal(t, binary) :: t
  def literal(combinator \\ empty(), binary)
      when is_combinator(combinator) and is_binary(binary) do
    [{:literal, binary} | combinator]
  end

  @doc """
  Ignores the output of combinator given in `to_ignore`.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :ignorable, literal("T") |> ignore() |> integer(2, 2)
      end

      MyParser.ignorable("T12")
      #=> {:ok, [12], "", 1, 3}

  """
  @spec ignore(t, t) :: t
  def ignore(combinator \\ empty(), to_ignore)
      when is_combinator(combinator) and is_combinator(to_ignore) do
    if to_ignore == empty() do
      to_ignore
    else
      quoted_traverse(combinator, to_ignore, fn _ -> [] end)
    end
  end

  @doc """
  Replaces the output of combinator given in `to_replace` by a single value.

  The `value` will be injected at the compile site
  and therefore must be escapable via `Macro.escape/1`.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :replaceable, literal("T") |> replace("OTHER") |> integer(2, 2)
      end

      MyParser.replaceable("T12")
      #=> {:ok, ["OTHER", 12], "", 1, 3}

  """
  @spec replace(t, t, term) :: t
  def replace(combinator \\ empty(), to_replace, value)
      when is_combinator(combinator) and is_combinator(to_replace) do
    value = Macro.escape(value)
    quoted_traverse(combinator, to_replace, fn _ -> [value] end)
  end

  @doc """
  Allow the combinator given on `to_repeat` to appear zero or more times.

  Beware! Since `repeat/2` allows zero entries, it cannot be used inside
  `choice/2`, because it will always succeed and may lead to unused function
  warnings since any further choice won't ever be attempted. For example,
  because `repeat/2` always succeeds, the `literal/2` combinator below it
  won't ever run:

      choice([
        repeat(ascii_char([?a..?z])),
        literal("OK")
      ])

  Instead of `repeat/2`, you may want to use `times/3` with the flags `:min`
  and `:max`.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :repeat_lower, repeat(ascii_char([?a..?z]))
      end

      MyParser.repeat_lower("abcd")
      #=> {:ok, [?a, ?b, ?c, ?d], "", 1, 5}

      MyParser.repeat_lower("1234")
      #=> {:ok, [], "1234", 1, 1}

  """
  @spec repeat(t, t) :: t
  def repeat(combinator \\ empty(), to_repeat)
      when is_combinator(combinator) and is_combinator(to_repeat) do
    to_repeat = reverse_combinators!(to_repeat, "repeat")
    [{:repeat, to_repeat} | combinator]
  end

  @doc """
  Allow the combinator given on `to_repeat` to appear at least, at most
  or exactly a given amout of times.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :minimum_lower, times(ascii_char([?a..?z]), min: 2)
      end

      MyParser.minimum_lower("abcd")
      #=> {:ok, [?a, ?b, ?c, ?d], "", 1, 5}

      MyParser.minimum_lower("ab12")
      #=> {:ok, [?a, ?b], "12", 1, 3}

      MyParser.minimum_lower("a123")
      #=> {:ok, [], "a123", 1, 1}

  """
  @spec times(t, t, pos_integer | [min_and_max]) :: t
  def times(combinator \\ empty(), to_repeat, count_or_min_max)

  def times(combinator, to_repeat, n)
      when is_combinator(combinator) and is_combinator(to_repeat) and is_integer(n) and n >= 1 do
    duplicate(combinator, to_repeat, n)
  end

  def times(combinator, to_repeat, opts)
      when is_combinator(combinator) and is_combinator(to_repeat) and is_list(opts) do
    {min, max} = validate_min_and_max!(opts)

    combinator =
      if min do
        duplicate(combinator, to_repeat, min)
      else
        combinator
      end

    to_repeat = reverse_combinators!(to_repeat, "times")

    combinator =
      if max do
        [{:repeat_up_to, to_repeat, max - (min || 0)} | combinator]
      else
        [{:repeat, to_repeat} | combinator]
      end

    combinator
  end

  @doc """
  Chooses one of the given combinators.

  Expects at leasts two choices.

  ## Beware! Char combinators

  Note both `utf8_char/2` and `ascii_char/2` allow multiple ranges to
  be given. Therefore, instead this:

      choice([
        ascii_char([?a..?z]),
        ascii_char([?A..?Z]),
      ])

  One should simply prefer:

      ascii_char([?a..?z, ?A..?Z])

  As the latter is compiled more efficiently by `NimbleParser`.

  ## Beware! Always successful combinators

  If a combinator that always succeeds is given as a choice, that choice
  will always succeed which may lead to unused function warnings since
  any further choice won't ever be attempted. For example, because `repeat/2`
  always succeeds, the `literal/2` combinator below it won't ever run:

      choice([
        repeat(ascii_char([?0..?9])),
        literal("OK")
      ])

  Instead of `repeat/2`, you may want to use `times/3` with the flags `:min`
  and `:max`.
  """
  @spec choice(t, t) :: t
  def choice(combinator \\ empty(), [_, _ | _] = choices) when is_combinator(combinator) do
    choices = Enum.map(choices, &Enum.reverse/1)
    [{:choice, choices} | combinator]
  end

  @doc """
  Marks the given combinator as `optional`.

  It is equivalent to `choice([optional, empty()])`.
  """
  @spec optional(t, t) :: t
  def optional(combinator \\ empty(), optional) do
    choice(combinator, [optional, empty()])
  end

  ## Helpers

  defp validate_min_and_max!(opts) do
    min = opts[:min]
    max = opts[:max]

    cond do
      min && max ->
        validate_min_or_max!(:min, min)
        validate_min_or_max!(:max, max)

        max <= min and
          raise ArgumentError,
                "expected :max to be strictly more than :min, got: #{min} and #{max}"

      min ->
        validate_min_or_max!(:min, min)

      max ->
        validate_min_or_max!(:max, max)

      true ->
        raise ArgumentError, "expected :min or :max to be given"
    end

    {min, max}
  end

  defp validate_min_or_max!(kind, value) do
    unless is_integer(value) and value >= 1 do
      raise ArgumentError, "expected #{kind} to be an integer more than 1, got: #{inspect(value)}"
    end
  end

  defp split_ranges!(ranges, context) do
    Enum.split_with(ranges, &split_range!(&1, context))
  end

  defp split_range!(x, _context) when is_integer(x), do: true
  defp split_range!(_.._, _context), do: true
  defp split_range!({:not, x}, _context) when is_integer(x), do: false
  defp split_range!({:not, _.._}, _context), do: false

  defp split_range!(range, context) do
    raise ArgumentError, "unknown range #{inspect(range)} given to #{context}"
  end

  defp compile_call!(arg, {module, function, args}, _context)
       when is_atom(module) and is_atom(function) and is_list(args) do
    quote do
      unquote(module).unquote(function)(unquote(arg), unquote_splicing(Macro.escape(args)))
    end
  end

  defp compile_call!(arg, {function, args}, _context) when is_atom(function) and is_list(args) do
    quote do
      unquote(function)(unquote(arg), unquote_splicing(Macro.escape(args)))
    end
  end

  defp compile_call!(_arg, unknown, context) do
    raise ArgumentError, "unknown call given to #{context}, got: #{inspect(unknown)}"
  end

  defp reverse_combinators!([], action) do
    raise ArgumentError, "cannot #{action} empty combinator"
  end

  defp reverse_combinators!(combinator, _action) when is_combinator(combinator) do
    Enum.reverse(combinator)
  end

  ## Inner combinators

  defp bin_segment(combinator, inclusive, exclusive, modifiers) do
    [{:bin_segment, inclusive, exclusive, modifiers} | combinator]
  end
end
