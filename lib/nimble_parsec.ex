# TODO: runtime_composition()
# TODO: integer()
# TODO: many()
# TODO: choice()
# TODO: Docs
# TODO: replace/3
# TODO: Private parsecs

defmodule NimbleParsec do
  defmacrop is_combinator(combinator) do
    quote do
      is_list(unquote(combinator))
    end
  end

  defmacro defparsec(name, combinator, opts \\ []) do
    quote bind_quoted: [name: name, combinator: combinator, opts: opts] do
      def unquote(name)(binary, opts \\ []) when is_binary(binary) do
        unquote(:"#{name}__0")(binary, [], [], 1, 1)
      end

      {defs, inline} = NimbleParsec.Compiler.compile(name, combinator, opts)
      @compile {:inline, inline}

      for {name, args, guards, body} <- defs do
        defp unquote(name)(unquote_splicing(args)) when unquote(guards), do: unquote(body)

        # IO.puts(Macro.to_string(quote do
        #   defp unquote(name)(unquote_splicing(args)) when unquote(guards), do: unquote(body)
        # end))
      end

      :ok
    end
  end

  @type t :: [combinator()]
  @type bit_modifiers :: [:signed | :unsigned | :native | :little | :big]
  @type call ::
          {module :: atom(), function :: atom(), args :: [term]}
          | {function :: atom(), args :: [term]}

  # Steps to add a new bound combinator:
  #
  #   1. Update the combinator type
  #   2. Update the compiler bound combinator step
  #   3. Update the compiler label step
  #
  @typep combinator ::
           {:literal, binary}
           | {:label, t, binary}
           | {:traverse, t, (Macro.t() -> Macro.t())}
           | {:compile_bit_integer, [Range.t()], bit_modifiers}
           | {:compile_traverse, t, (Macro.t() -> Macro.t()), (Macro.t() -> Macro.t())}

  @doc ~S"""
  Returns an empty combinator.

  An empty combinator cannot be compiled on its own.
  """
  def empty() do
    []
  end

  @doc ~S"""
  Defines a single ascii codepoint in the given ranges.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :digit_and_lowercase,
                  empty()
                  |> ascii_codepoint([?0..?9])
                  |> ascii_codepoint([?a..?z])
      end

      MyParser.digit_and_lowercase("1a")
      #=> {:ok, [?1, ?a], "", 1, 3}

      MyParser.digit_and_lowercase("a1")
      #=> {:error, "expected a byte in the range ?0..?9, followed by a byte in the range ?a..?z", "a1", 1, 1}

  """
  def ascii_codepoint(combinator \\ empty(), ranges) do
    if ranges == [] or Enum.any?(ranges, &(?\n in &1)) do
      # TODO: Implement this.
      raise ArgumentError,
            "empty ranges or ranges with newlines in them are not currently supported"
    else
      compile_bit_integer(combinator, ranges, [])
    end
  end

  @doc ~S"""
  Adds a label to the combinator to be used in error reports.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :digit_and_lowercase,
                  empty()
                  |> ascii_codepoint([?0..?9])
                  |> ascii_codepoint([?a..?z])
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
  Defines an integer combinator with `min` and `max` length.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :two_digits_integer, integer(2, 2)
      end

      MyParser.two_digits_integer("123")
      #=> {:ok, [12], "3", 1, 3}

      MyParser.two_digits_integer("1a3")
      #=> {:error, "expected a two digits integer", "1a3", 1, 1}

  """
  def integer(combinator \\ empty(), min, max)

  def integer(combinator, size, size)
      when is_integer(size) and size > 0 and is_combinator(combinator) do
    integer =
      Enum.reduce(1..size, empty(), fn _, acc ->
        compile_bit_integer(acc, [?0..?9], [])
      end)

    mapped = compile_traverse(empty(), integer, &from_ascii_to_integer/1)
    label(combinator, mapped, "#{size} digits integer")
  end

  def integer(combinator, min, max)
      when is_integer(min) and min > 0 and is_integer(max) and max >= min and
             is_combinator(combinator) do
    # TODO: Implement variadic size integer.
    raise ArgumentError, "not yet implemented"
  end

  defp from_ascii_to_integer(vars) do
    vars
    |> from_ascii_to_integer(1)
    |> Enum.reduce(&{:+, [], [&2, &1]})
    |> List.wrap()
  end

  defp from_ascii_to_integer([var | vars], index) do
    [quote(do: (unquote(var) - ?0) * unquote(index)) | from_ascii_to_integer(vars, index * 10)]
  end

  defp from_ascii_to_integer([], _index) do
    []
  end

  @doc ~S"""
  Concatenates two combinators.

  ## Examples

      defmodule MyParser do
        import NimbleParsec

        defparsec :digit_upper_lower_plus,
                  concat(
                    concat(ascii_codepoint([?0..?9]), ascii_codepoint([?A..?Z])),
                    concat(ascii_codepoint([?a..?z]), ascii_codepoint([?+..?+]))
                  )
      end

      MyParser.digit_upper_lower_plus("1Az+")
      #=> {:ok, [?1, ?A, ?z, ?+], "", 1, 5}

  """
  def concat(left, right) when is_combinator(left) and is_combinator(right) do
    right ++ left
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

        defparsec :letters_to_codepoints,
                  ascii_codepoint([?a..?z])
                  |> ascii_codepoint([?a..?z])
                  |> ascii_codepoint([?a..?z])
                  |> traverse({:join_and_wrap, ["-"]})

        defp join_and_wrap(args, joiner) do
          args |> Enum.join(joiner) |> List.wrap()
        end
      end

      MyParser.letters_to_codepoints("abc")
      #=> {:ok, ["99-98-97"], "", 1, 4}
  """
  @spec traverse(t, t, call) :: t
  def traverse(combinator \\ empty(), to_traverse, call)
      when is_combinator(combinator) and is_combinator(to_traverse) and is_tuple(call) do
    to_traverse = reverse_combinators!(to_traverse, "traverse")
    compile_call!(:ok, call, "traverse")
    runtime_traverse(combinator, to_traverse, &compile_call!(&1, call, "traverse"))
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

        defparsec :letters_to_string_codepoints,
                  ascii_codepoint([?a..?z])
                  |> ascii_codepoint([?a..?z])
                  |> ascii_codepoint([?a..?z])
                  |> map({Integer, :to_string, []})
      end

      MyParser.letters_to_string_codepoints("abc")
      #=> {:ok, ["97", "98", "99"], "", 1, 4}
  """
  @spec map(t, t, call) :: t
  def map(combinator \\ empty(), to_map, call)
      when is_combinator(combinator) and is_combinator(to_map) and is_tuple(call) do
    to_map = reverse_combinators!(to_map, "map")
    var = Macro.var(:var, __MODULE__)
    call = compile_call!(var, call, "map")

    runtime_traverse(combinator, to_map, fn arg ->
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

        defparsec :letters_to_reduced_codepoints,
                  ascii_codepoint([?a..?z])
                  |> ascii_codepoint([?a..?z])
                  |> ascii_codepoint([?a..?z])
                  |> reduce({Enum, :join, ["-"]})
      end

      MyParser.letters_to_reduced_codepoints("abc")
      #=> {:ok, ["97-98-99"], "", 1, 4}
  """
  @spec reduce(t, t, call) :: t
  def reduce(combinator \\ empty(), to_reduce, call)
      when is_combinator(combinator) and is_combinator(to_reduce) and is_tuple(call) do
    to_reduce = reverse_combinators!(to_reduce, "reduce")
    compile_call!(:ok, call, "reduce")

    runtime_traverse(combinator, to_reduce, fn arg ->
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
  def ignore(combinator \\ empty(), to_ignore)

  def ignore(combinator, to_ignore) when is_combinator(combinator) and is_combinator(to_ignore) do
    to_ignore = reverse_combinators!(to_ignore, "ignore")
    # TODO: Define the runtime behaviour.
    compile_traverse(combinator, to_ignore, fn _ -> [] end)
  end

  ## Inner combinators

  # A runtime traverse. Notice the `to_traverse` inside the
  # combinator is already expected to be reversed.
  defp runtime_traverse(combinator, to_traverse, traversal) when is_function(traversal, 1) do
    [{:traverse, to_traverse, traversal} | combinator]
  end

  # A compile traverse may or may not be expanded at runtime
  # as it depends if `to_traverse` is also bound. For this
  # reason, some operators may pass a runtime_fun/1. If one
  # is not passed, it is assumed that the behaviour is
  # guaranteed to be bound.
  #
  # Notice the `to_traverse` inside the combinator is already
  # expected to be reversed.
  defp compile_traverse(combinator, to_traverse, compile_fun, runtime_fun \\ &always_raise!/1) do
    [{:compile_traverse, to_traverse, compile_fun, runtime_fun} | combinator]
  end

  # A compile bit integer is verified to not have a newline on it
  # and is always bound.
  defp compile_bit_integer(combinator, [_ | _] = ranges, modifiers) do
    [{:compile_bit_integer, ranges, modifiers} | combinator]
  end

  defp reverse_combinators!([], action) do
    raise ArgumentError, "cannot #{action} empty combinator"
  end

  defp reverse_combinators!(combinators, _action) do
    Enum.reverse(combinators)
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

  defp always_raise!(_) do
    raise "this function must never be invoked"
  end
end
