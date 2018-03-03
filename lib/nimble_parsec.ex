# TODO: Make ignore cascade up.
# TODO: Test debug, wrap, tag, repeat_until/6, repeat_while/6, traverse with error.
# TODO: Add ascii_string / utf8_string

defmodule NimbleParsec do
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  defmacrop is_combinator(combinator) do
    quote do
      is_list(unquote(combinator))
    end
  end

  @doc """
  Defines a public parser `combinator` with the given `name` and `opts`.

  ## Beware!

  `defparsec/3` is executed during compilation. This means you can't
  invoke a function defined in the same module. The following will error
  because the `date` function has not yet been defined:

      defmodule MyParser do
        import NimbleParsec

        def date do
          integer(4)
          |> ignore(string("-"))
          |> integer(2)
          |> ignore(string("-"))
          |> integer(2)
        end

        defparsec :date, date()
      end

  This can be solved in different ways. You may define `date` in another
  module and then invoke it. You can also store the parsec in a variable
  or a module attribute and use that instead. For example:

      defmodule MyParser do
        import NimbleParsec

        date =
          integer(4)
          |> ignore(string("-"))
          |> integer(2)
          |> ignore(string("-"))
          |> integer(2)

        defparsec :date, date
      end

  ## Options

    * `:inline` - when true, inlines clauses that work as redirection for
      other clauses. It is disabled by default because of a bug in Elixir
      v1.5 and v1.6 where unused functions that are inlined cause a
      compilation error

    * `:debug` - when true, writes generated clauses to `:stderr` for debugging

  """
  defmacro defparsec(name, combinator, opts \\ []) do
    fun =
      quote bind_quoted: [name: name] do
        @doc """
        Parses the given `binary` as #{name}.

        Returns `{:ok, [token], rest, context, line, byte_offset}` or
        `{:error, reason, rest, context, line, byte_offset}`.

        ## Options

          * `:line` - the initial line, defaults to 1
          * `:byte_offset` - the initial byte offset, defaults to 0
          * `:context` - the initial context value. It will be converted
            to a map

        """
        @spec unquote(name)(binary, keyword) ::
                {:ok, [term], rest, context, line, byte_offset}
                | {:error, reason, rest, context, line, byte_offset}
              when line: {pos_integer, byte_offset},
                   byte_offset: pos_integer,
                   rest: binary,
                   reason: String.t(),
                   context: map()
        def unquote(name)(binary, opts \\ []) when is_binary(binary) do
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
      end

    quote do
      unquote(fun)
      unquote(compile(name, combinator, opts))
    end
  end

  @doc """
  Defines a private parser combinator.

  It cannot be invoked directly, only via `parsec/2`.

  Receives the same options as `defparsec/3`.
  """
  defmacro defparsecp(name, combinator, opts \\ []) do
    compile(name, combinator, opts)
  end

  defp compile(name, combinator, opts) do
    quote bind_quoted: [name: name, combinator: combinator, opts: opts] do
      {defs, inline} = NimbleParsec.Compiler.compile(name, combinator, opts)

      if inline != [] do
        @compile {:inline, inline}
      end

      for {name, args, guards, body} <- defs do
        defp unquote(name)(unquote_splicing(args)) when unquote(guards), do: unquote(body)
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
  @type call :: mfargs | fargs | atom
  @type mfargs :: {module, atom, args :: [term]}
  @type fargs :: {atom, args :: [term]}

  # Steps to add a new bound combinator:
  #
  #   1. Update the combinator type
  #   2. Update the compiler bound combinator step
  #   3. Update the compiler label step
  #
  @typep combinator :: bound_combinator | maybe_bound_combinator | unbound_combinator

  @typep bound_combinator ::
           {:bin_segment, [inclusive_range], [exclusive_range], [bin_modifiers]}
           | {:string, binary}

  @typep maybe_bound_combinator ::
           {:label, t, binary}
           | {:traverse, t, [mfargs]}

  @typep unbound_combinator ::
           {:choice, [t]}
           | {:parsec, atom}
           | {:repeat, mfargs, t, t, mfargs}
           | {:repeat_up_to, t, pos_integer}

  @cont_context {__MODULE__, :__cont_context__, []}

  @doc ~S"""
  Returns an empty combinator.

  An empty combinator cannot be compiled on its own.
  """
  def empty() do
    []
  end

  @doc """
  Invokes an already compiled parsec with name `name` in the
  same module.

  It is useful to implement recursive definitions.

  It can also be used to exchange compilation time by runtime
  performance. If you have a parser used over and over again,
  you can compile it using `defparsecp` and rely on it via
  this function. The tree size built at compile time will be
  reduce although runtime performance is degraded as every time
  this function is invoked it introduces a stacktrace entry.
  """
  def parsec(combinator \\ empty(), name) when is_combinator(combinator) and is_atom(name) do
    [{:parsec, name} | combinator]
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
    non_empty!(to_label, "label")
    [{:label, Enum.reverse(to_label), label} | combinator]
  end

  @doc ~S"""
  Defines an integer combinator with of exact length or `min` and `max`
  length.

  If you want an integer of unknown size, use `integer(min: 1)`.

  This combinator does not parse the sign and is always on base 10.

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
    quoted_traverse(combinator, integer, {__MODULE__, :__compile_integer__, []})
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

    quoted_traverse(combinator, integer, {__MODULE__, :__runtime_integer__, []})
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

  @doc """
  Puts the result of the given combinator as the first element
  of a tuple with the `byte_offset` as second element.

  `byte_offset` is a non-negative integer.
  """
  @spec byte_offset(t, t) :: t
  def byte_offset(combinator \\ empty(), to_wrap)
      when is_combinator(combinator) and is_combinator(to_wrap) do
    quoted_traverse(combinator, to_wrap, {__MODULE__, :__byte_offset__, []})
  end

  @doc """
  Puts the result of the given combinator as the first element
  of a tuple with the `line` as second element.

  `line` is a tuple where the first element is the current line
  and the second element is the byte offset immediately after
  the newline.
  """
  @spec line(t, t) :: t
  def line(combinator \\ empty(), to_wrap)
      when is_combinator(combinator) and is_combinator(to_wrap) do
    quoted_traverse(combinator, to_wrap, {__MODULE__, :__line__, []})
  end

  @doc ~S"""
  Traverses the combinator results with the remote or local function `call`.

  `call` is either a `{module, function, args}` representing
  a remote call, a `{function, args}` representing a local call
  or an atom `function` representing `{function, []}`.

  The parser results to be traversed, the parser context, the
  current line and the current offset will be prepended to the
  given `args`. The `args` will be injected at the compile site
  and therefore must be escapable via `Macro.escape/1`.

  The `call` must return a tuple with list of results to be added
  to the accumulator as first argument and a context as second
  argument. It may also return `{:error, reason}` to stop processing.
  Notice the received results are in reverse order and must be
  returned in reverse order too.

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

        defp join_and_wrap(args, context, _line, _offset, joiner) do
          {args |> Enum.join(joiner) |> List.wrap(), context}
        end
      end

      MyParser.letters_to_chars("abc")
      #=> {:ok, ["99-98-97"], "", 1, 4}

  """
  @spec traverse(t, t, call) :: t
  def traverse(combinator \\ empty(), to_traverse, call)
      when is_combinator(combinator) and is_combinator(to_traverse) do
    compile_call!([], call, "traverse")
    quoted_traverse(combinator, to_traverse, {__MODULE__, :__call__, [call, "traverse"]})
  end

  @doc """
  Invokes `call` to emit the AST that traverses the `to_traverse`
  combinator results.

  `call` is a `{module, function, args}`. The AST representation
  of the parser results, context, line and offset will be prepended
  to `args`. `call` is invoked at compile time and is useful in
  combinators that avoid injecting runtime dependencies.

  The `call` must return a list of results to be added to
  the accumulator. Notice the received results are in reverse
  order and must be returned in reverse order too.

  The number of elements returned does not need to be
  the same as the number of elements given.
  """
  @spec quoted_traverse(t, t, mfargs) :: t
  def quoted_traverse(combinator, to_traverse, {_, _, _} = call)
      when is_combinator(combinator) and is_combinator(to_traverse) do
    case to_traverse do
      [{:traverse, inner_traverse, inner_call}] ->
        [{:traverse, inner_traverse, [call | inner_call]} | combinator]

      _ ->
        [{:traverse, Enum.reverse(to_traverse), [call]} | combinator]
    end
  end

  @doc ~S"""
  Maps over the combinator results with the remote or local function in `call`.

  `call` is either a `{module, function, args}` representing
  a remote call, a `{function, args}` representing a local call
  or an atom `function` representing `{function, []}`.

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
      when is_combinator(combinator) and is_combinator(to_map) do
    var = Macro.var(:var, __MODULE__)
    call = compile_call!([var], call, "map")
    quoted_traverse(combinator, to_map, {__MODULE__, :__map__, [var, call]})
  end

  @doc ~S"""
  Reduces over the combinator results with the remote or local function in `call`.

  `call` is either a `{module, function, args}` representing
  a remote call, a `{function, args}` representing a local call
  or an atom `function` representing `{function, []}`.

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
      when is_combinator(combinator) and is_combinator(to_reduce) do
    compile_call!([], call, "reduce")
    quoted_traverse(combinator, to_reduce, {__MODULE__, :__reduce__, [call]})
  end

  @doc """
  Wraps the results of the given combinator in `to_wrap` in a list.
  """
  @spec wrap(t, t) :: t
  def wrap(combinator \\ empty(), to_wrap)
      when is_combinator(combinator) and is_combinator(to_wrap) do
    quoted_traverse(combinator, to_wrap, {__MODULE__, :__wrap__, []})
  end

  @doc """
  Tags the result of the given combinator in `to_tag` in a tuple with
  `tag` as first element.
  """
  @spec tag(t, t) :: t
  def tag(combinator \\ empty(), to_tag, tag)
      when is_combinator(combinator) and is_combinator(to_tag) do
    quoted_traverse(combinator, to_tag, {__MODULE__, :__tag__, [Macro.escape(tag)]})
  end

  @doc """
  Inspects the combinator state given to `to_debug` with the given `opts`.
  """
  @spec debug(t, t) :: t
  def debug(combinator \\ empty(), to_debug)
      when is_combinator(combinator) and is_combinator(to_debug) do
    quoted_traverse(combinator, to_debug, {__MODULE__, :__debug__, []})
  end

  @doc ~S"""
  Defines a string binary value.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :string_t, string("T")
      end

      MyParser.string_t("T")
      #=> {:ok, ["T"], "", 1, 2}

      MyParser.string_t("not T")
      #=> {:error, "expected a string \"T\"", "not T", 1, 1}

  """
  @spec string(t, binary) :: t
  def string(combinator \\ empty(), binary)
      when is_combinator(combinator) and is_binary(binary) do
    [{:string, binary} | combinator]
  end

  @doc """
  Ignores the output of combinator given in `to_ignore`.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :ignorable, string("T") |> ignore() |> integer(2, 2)
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
      quoted_traverse(combinator, to_ignore, {__MODULE__, :__constant__, [[]]})
    end
  end

  @doc """
  Replaces the output of combinator given in `to_replace` by a single value.

  The `value` will be injected at the compile site
  and therefore must be escapable via `Macro.escape/1`.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :replaceable, string("T") |> replace("OTHER") |> integer(2, 2)
      end

      MyParser.replaceable("T12")
      #=> {:ok, ["OTHER", 12], "", 1, 3}

  """
  @spec replace(t, t, term) :: t
  def replace(combinator \\ empty(), to_replace, value)
      when is_combinator(combinator) and is_combinator(to_replace) do
    value = Macro.escape(value)
    quoted_traverse(combinator, to_replace, {__MODULE__, :__constant__, [[value]]})
  end

  @doc """
  Allow the combinator given on `to_repeat` to appear zero or more times.

  Beware! Since `repeat/2` allows zero entries, it cannot be used inside
  `choice/2`, because it will always succeed and may lead to unused function
  warnings since any further choice won't ever be attempted. For example,
  because `repeat/2` always succeeds, the `string/2` combinator below it
  won't ever run:

      choice([
        repeat(ascii_char([?a..?z])),
        string("OK")
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
    non_empty!(to_repeat, "repeat")
    quoted_repeat_while(combinator, to_repeat, {__MODULE__, :__cont_context__, []})
  end

  @doc ~S"""
  Repeats while the given remote or local function `while` returns
  `{:cont, context}`.

  In case repetition should stop, `while` must return `{:break, context}`.

  `while` is either a `{module, function, args}` representing
  a remote call, a `{function, args}` representing a local call
  or an atom `function` representing `{function, []}`.

  The `rest` of the binary to be parsed, the parser context, the
  current line and the current offset will be prepended to the
  given `args`. The `args` will be injected at the compile site
  and therefore must be escapable via `Macro.escape/1`.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :string,
                  ascii_char([?"])
                  |> repeat_while(
                    choice([
                      ~S(\") |> string() |> replace(?"),
                      utf8_char([])
                    ]),
                    {:not_quote, []}
                  )
                  |> ascii_char([?"])
                  |> reduce({List, :to_string, []})

        defp not_quote(<<?", _::binary>>, context, _, _), do: {:stop, context}
        defp not_quote(_, _, _, _), do: {:cont, context}
      end

      MyParser.string(~S("string with quotes \" inside"))
      {:ok, ["\"string with quotes \" inside\""], "", 1, 31}

  """
  @spec repeat_while(t, t, call) :: t
  def repeat_while(combinator \\ empty(), to_repeat, while)
      when is_combinator(combinator) and is_combinator(to_repeat) do
    non_empty!(to_repeat, "repeat_while")
    compile_call!([], while, "repeat_while")
    quoted_repeat_while(combinator, to_repeat, {__MODULE__, :__call__, [while, "repeat_while"]})
  end

  @doc ~S"""
  Repeats `to_repeat` until one of the combinators in `choices` match.

  Each of the combinators given in choice must be optimizable into
  a single pattern, otherwise this function will refuse to compile.
  Use `repeat_while/3` for a general mechanism for repeating.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :string,
                  ascii_char([?"])
                  |> repeat_until(
                    choice([
                      ~S(\") |> string() |> replace(?"),
                      utf8_char([])
                    ]),
                    [ascii_char(?")]
                  )
                  |> ascii_char([?"])
                  |> reduce({List, :to_string, []})

        defp not_quote(<<?", _::binary>>), do: false
        defp not_quote(_), do: true
      end

      MyParser.string(~S("string with quotes \" inside"))
      {:ok, ["\"string with quotes \" inside\""], "", 1, 31}

  """
  @spec repeat_until(t, t, [t]) :: t
  def repeat_until(combinator \\ empty(), to_repeat, [_ | _] = choices)
      when is_combinator(combinator) and is_combinator(to_repeat) and is_list(choices) do
    non_empty!(to_repeat, "repeat_until")
    clauses = check_until_choices!(choices)
    quoted_repeat_while(combinator, to_repeat, {__MODULE__, :__repeat_until__, [clauses]})
  end

  @doc """
  Invokes `while` to emit the AST that will repeat `to_repeat`
  while the AST code returns `{:cont, context}`.

  In case repetition should stop, `while` must return `{:halt, context}`.

  `while` is a `{module, function, args}` where the AST representations
  of the binary to be parsed, context, line and offset will be
  prended to `args`. `while` is invoked at compile time and is
  useful in combinators that avoid injecting runtime dependencies.
  """
  @spec quoted_repeat_while(t, t, mfargs) :: t
  def quoted_repeat_while(combinator \\ empty(), to_repeat, {_, _, _} = while)
      when is_combinator(combinator) and is_combinator(to_repeat) do
    non_empty!(to_repeat, "quoted_repeat_while")
    [{:repeat, @cont_context, [], Enum.reverse(to_repeat), while} | combinator]
  end

  @spec repeat_while(t, call, t, t, call) :: t
  def repeat_while(combinator \\ empty(), init, prelude, to_repeat, while)
      when is_combinator(combinator) and is_combinator(prelude) and is_combinator(to_repeat) do
    non_empty!(prelude, "repeat_while")
    non_empty!(to_repeat, "repeat_while")
    compile_call!([], init, "repeat_while")
    compile_call!([], while, "repeat_while")

    quoted_repeat_while(
      combinator,
      {__MODULE__, :__call__, [init, "repeat_while"]},
      prelude,
      to_repeat,
      {__MODULE__, :__call__, [while, "repeat_while"]}
    )
  end

  @spec repeat_until(t, call, t, t, [t]) :: t
  def repeat_until(combinator \\ empty(), init, prelude, to_repeat, [_ | _] = choices)
      when is_combinator(combinator) and is_combinator(prelude) and is_combinator(to_repeat) and
             is_list(choices) do
    compile_call!([], init, "repeat_until")
    non_empty!(prelude, "repeat_until")
    non_empty!(to_repeat, "repeat_until")
    clauses = check_until_choices!(choices)

    quoted_repeat_while(
      combinator,
      {__MODULE__, :__call__, [init, "repeat_until"]},
      prelude,
      to_repeat,
      {__MODULE__, :__repeat_until__, [clauses]}
    )
  end

  @spec quoted_repeat_while(t, mfargs, t, t, mfargs) :: t
  def quoted_repeat_while(
        combinator \\ empty(),
        {_, _, _} = init,
        prelude,
        to_repeat,
        {_, _, _} = while
      )
      when is_combinator(combinator) and is_combinator(prelude) and is_combinator(to_repeat) do
    non_empty!(prelude, "quoted_repeat_while")
    non_empty!(to_repeat, "quoted_repeat_while")
    [{:repeat, init, Enum.reverse(prelude), Enum.reverse(to_repeat), while} | combinator]
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
    non_empty!(to_repeat, "times")
    duplicate(combinator, to_repeat, n)
  end

  def times(combinator, to_repeat, opts)
      when is_combinator(combinator) and is_combinator(to_repeat) and is_list(opts) do
    {min, max} = validate_min_and_max!(opts)
    non_empty!(to_repeat, "times")

    combinator =
      if min do
        duplicate(combinator, to_repeat, min)
      else
        combinator
      end

    to_repeat = Enum.reverse(to_repeat)

    combinator =
      if max do
        [{:repeat_up_to, to_repeat, max - (min || 0)} | combinator]
      else
        [{:repeat, @cont_context, [], to_repeat, @cont_context} | combinator]
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
  always succeeds, the `string/2` combinator below it won't ever run:

      choice([
        repeat(ascii_char([?0..?9])),
        string("OK")
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

  defp compile_call!(extra, {module, function, args}, _context)
       when is_atom(module) and is_atom(function) and is_list(args) do
    quote do
      unquote(module).unquote(function)(
        unquote_splicing(extra),
        unquote_splicing(Macro.escape(args))
      )
    end
  end

  defp compile_call!(extra, {function, args}, _context)
       when is_atom(function) and is_list(args) do
    quote do
      unquote(function)(unquote_splicing(extra), unquote_splicing(Macro.escape(args)))
    end
  end

  defp compile_call!(extra, function, _context) when is_atom(function) do
    quote do
      unquote(function)(unquote_splicing(extra))
    end
  end

  defp compile_call!(_args, unknown, context) do
    raise ArgumentError, "unknown call given to #{context}, got: #{inspect(unknown)}"
  end

  defp non_empty!([], action),
    do: raise(ArgumentError, "cannot call #{action} on empty combinator")

  defp non_empty!(combinator, _action), do: combinator

  defp check_until_choices!(choices) do
    for choice <- choices do
      if choice == [] do
        raise "cannot pass empty combinator as choice in repeat_until"
      end

      case NimbleParsec.Compiler.compile_pattern(choice) do
        {_inputs, _guards} = pair -> pair
        :error -> raise "cannot compile combinator as choice given in repeat_until"
      end
    end
  end

  ## Inner combinators

  defp bin_segment(combinator, inclusive, exclusive, modifiers) do
    [{:bin_segment, inclusive, exclusive, modifiers} | combinator]
  end

  ## Callbacks functions

  @doc false
  def __call__(quoted, context, line, offset, call, combinator) do
    compile_call!([quoted, context, line, offset], call, combinator)
  end

  @doc false
  def __wrap__(acc, context, _line, _offset) do
    {[reverse_now_or_later(acc)], context}
  end

  @doc false
  def __tag__(acc, context, _line, _offset, tag) do
    {[{tag, reverse_now_or_later(acc)}], context}
  end

  @doc false
  def __debug__(acc, context, line, offset) do
    quote bind_quoted: [acc: acc, context: context, line: line, offset: offset] do
      IO.puts("""
      == DEBUG ==
      Acc: #{inspect(:lists.reverse(acc))}
      Ctx: #{inspect(context)}
      Lin: #{inspect(line)}
      Off: #{inspect(offset)}
      """)

      {acc, context}
    end
  end

  @doc false
  def __constant__(_acc, context, _line, _offset, constant) do
    {constant, context}
  end

  @doc false
  def __line__(acc, context, line, _offset) do
    {[{reverse_now_or_later(acc), line}], context}
  end

  @doc false
  def __byte_offset__(acc, context, _line, offset) do
    {[{reverse_now_or_later(acc), offset}], context}
  end

  @doc false
  def __map__(acc, context, _line, _offset, var, call) do
    ast =
      quote do
        Enum.map(unquote(acc), fn unquote(var) -> unquote(call) end)
      end

    {ast, context}
  end

  @doc false
  def __reduce__(acc, context, _line, _offset, call) do
    {[compile_call!([reverse_now_or_later(acc)], call, "reduce")], context}
  end

  @doc false
  def __cont_context__(_rest, context, _line, _offset) do
    {:cont, context}
  end

  @doc false
  def __repeat_until__(rest, context, _line, _offset, clauses) do
    clauses =
      for {inputs, guards} <- clauses do
        hd(
          quote do
            <<unquote_splicing(inputs), _::binary>> when unquote(guards) ->
              {:halt, unquote(context)}
          end
        )
      end

    clauses = clauses ++ quote(do: (_ -> {:cont, unquote(context)}))

    quote do
      case unquote(rest), do: unquote(clauses)
    end
  end

  @doc false
  def __runtime_integer__(acc, context, _line, _offset) do
    ast =
      quote do
        [head | tail] = :lists.reverse(unquote(acc))
        [:lists.foldl(fn x, acc -> x - ?0 + acc * 10 end, head, tail)]
      end

    {ast, context}
  end

  @doc false
  def __compile_integer__(acc, context, _line, _offset) when is_list(acc) do
    ast =
      acc
      |> quoted_ascii_to_integer(1)
      |> Enum.reduce(&{:+, [], [&2, &1]})

    {[ast], context}
  end

  defp reverse_now_or_later(list) when is_list(list), do: :lists.reverse(list)
  defp reverse_now_or_later(expr), do: quote(do: :lists.reverse(unquote(expr)))

  defp quoted_ascii_to_integer([var | vars], index) do
    [quote(do: (unquote(var) - ?0) * unquote(index)) | quoted_ascii_to_integer(vars, index * 10)]
  end

  defp quoted_ascii_to_integer([], _index) do
    []
  end
end
