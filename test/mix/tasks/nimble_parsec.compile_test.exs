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
        defparsec :parse, integer(2)
        defparsecp :parsep, integer(2)
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
        assert contents =~ "defp parsep__0("
        refute contents =~ "defp combinatorp(binary, opts \\\\ [])"
        assert contents =~ "defp combinatorp__0("
        assert contents =~ "  _pos = :ok\nend"
      end)

      # Ensure the output is also compilable.
      Mix.Task.run("compile")
    end)
  end
end
