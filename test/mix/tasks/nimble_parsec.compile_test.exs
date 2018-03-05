defmodule Mix.Tasks.NimbleParsec.CompileTest do
  use ExUnit.Case

  import Support.FileHelpers

  test "run" do
    in_tmp(fn _path ->
      File.mkdir_p!("lib")

      File.write!("lib/my_parser.ex.eex", """
      defmodule Mix.Tasks.NimbleParsec.CompileTest.Parser do
        <%= defparsec :parse, integer(2) %>
      end
      """)

      Mix.Task.run("nimble_parsec.compile", ["lib/my_parser.ex.eex"])
      assert_received {:mix_shell, :info, ["Generating lib/my_parser.ex"]}

      assert_file("lib/my_parser.ex", fn contents ->
        assert contents =~ "# Generated from lib/my_parser.ex.eex, do not edit."
        assert contents =~ "defmodule Mix.Tasks.NimbleParsec.CompileTest.Parser do"
        assert contents =~ "def parse(binary, opts \\\\ [])"
      end)

      Mix.Task.run("compile")
    end)
  end
end
