defmodule Mix.Tasks.NimbleParsec.CompileTest do
  use ExUnit.Case

  import Support.FileHelpers

  test "run" do
    in_tmp(fn _path ->
      File.mkdir_p!("lib")

      File.write!("lib/my_parser.ex.exs", """
      defmodule Mix.Tasks.NimbleParsec.CompileTest.Parser do
        _pre = :ok

        # parsec:Mix.Tasks.NimbleParsec.CompileTest.Parser

        import NimbleParsec

        nested =
          [string("am"), string("pm")]
          |> choice()
          |> lookahead_not(ascii_char([?s, ?p]))
          |> unwrap_and_tag(:nested)

        defparsec :lookahead_not_warning, string("foo") |> concat(optional(nested))

        defparsec :parse, integer(2)
        defparsecp :parsep, integer(2)
        defcombinator :combinator, integer(2)
        defcombinatorp :combinatorp, integer(2)

        # parsec:Mix.Tasks.NimbleParsec.CompileTest.Parser

        _pos = :ok
      end
      """)

      Mix.Task.run("nimble_parsec.compile", ["lib/my_parser.ex.exs"])
      assert_received {:mix_shell, :info, ["Generating lib/my_parser.ex"]}

      assert_file("lib/my_parser.ex", fn contents ->
        assert contents =~ "# Generated from lib/my_parser.ex.exs, do not edit."
        assert contents =~ "defmodule Mix.Tasks.NimbleParsec.CompileTest.Parser do\n  _pre = :ok"
        assert contents =~ "def parse(binary, opts \\\\ [])"
        assert contents =~ "defp parse__0("
        assert contents =~ "defp parsep(binary, opts \\\\ [])"
        assert contents =~ "def lookahead_not_warning(binary, opts \\\\ [])"
        assert contents =~ "defp parsep__0("
        refute contents =~ "def combinator(binary, opts \\\\ [])"
        assert contents =~ "def combinator__0("
        refute contents =~ "defp combinatorp(binary, opts \\\\ [])"
        assert contents =~ "defp combinatorp__0("
        assert contents =~ "  _pos = :ok\nend"
      end)

      # Ensure the output is also compilable.
      Mix.Task.run("compile")
    end)
  end

  describe "catching compilation time errors" do
    test "assure bad params are caught" do
      assert_raise Mix.Error, &run_task_with_two_files/0
    end

    test "assure gen_weights error is caught in choice" do
      """
          defparsecp :weighted_choice,
                  repeat(choice(
                  ascii_char([?X]),
                  [ascii_char([?a..?z]), ascii_char([?a..?z])],
                  gen_weights: [1]))
      """
      |> assert_compilation_error(
        1,
        ArgumentError,
        ":gen_weights must be a list of the same size as choices"
      )
    end

    test "assure invalid min max values are caught" do
      """
          defparsecp :bad_integer, integer(min: 2, max: 1)
      """
      |> assert_compilation_error(
        2,
        ArgumentError,
        "expected :max to be strictly greater than :min, got: 2 and 1"
      )
    end

    test "assure min or max to be given" do
      """
          defparsec :missing_constraint, ascii_char([]) |> times(string("foo"), [])
      """
      |> assert_compilation_error(3, ArgumentError, "expected :min or :max to be given")
    end

    test "assure max is old enough" do
      """
          defparsec :max_zero, integer(max: 0)
      """
      |> assert_compilation_error(
        4,
        ArgumentError,
        "expected max to be an integer greater than or equal to 1, got: 0"
      )
    end

    test "assure correct ranges" do
      """
          defparsec :bad_range, ascii_char([{:in, 1..2}])
      """
      |> assert_compilation_error(
        5,
        ArgumentError,
        "unknown range {:in, 1..2} given to ascii_char"
      )
    end

    test "unknown call" do
      """
          defparsec :bad_call,
                    integer(2)
                    |> post_traverse(string("0"), [__MODULE__, :dummy, []])
      """
      |> assert_compilation_error(
        6,
        ArgumentError,
        "unknown call given to post_traverse, got: [Mix.Tasks.NimbleParsec6.CompileTest.Parser, :dummy, []]"
      )
    end

    test "action on empty" do
      """
          defparsec :ad_nauseam, repeat(empty())
      """
      |> assert_compilation_error(7, ArgumentError, "cannot call repeat on empty combinator")
    end

    test "list non empty?" do
      """
          defparsec :listy, repeat([integer(1)])
      """
      |> assert_compilation_error(
        8,
        ArgumentError,
        "invalid combinator given to repeat, got a list of combinators instead"
      )
    end

    defp assert_compilation_error(code, nb, exception, message) do
      assert_raise exception, message, fn ->
        make_and_compile_module(nb, code)
      end
    end

    defp make_and_compile_module(nb, code) do
      module_code = """
        defmodule Mix.Tasks.NimbleParsec#{nb}.CompileTest.Parser do
          import NimbleParsec
          #{code}
        end
      """

      in_tmp(fn _path ->
        File.mkdir_p!("lib")

        File.write!("lib/my_parser#{nb}.ex.exs", module_code)
        Mix.Task.run("nimble_parsec.compile", ["lib/my_parser#{nb}.ex.exs"])
      end)
    end
  end

  defp run_task_with_two_files do
    Mix.Task.run("nimble_parsec.compile", ["lib/my_parser.ex.exs", "lib/my_parser.ex.exs"])
  end
end
