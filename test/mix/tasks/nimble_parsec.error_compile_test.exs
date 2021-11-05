defmodule Mix.Tasks.NimbleParsec.ErrorCompileTest do
  use ExUnit.Case

  import Support.FileHelpers

  test "min > max in integer" do
    filename = "lib/min_gt_max.ex.exs"

    in_tmp(fn _path ->
      File.mkdir_p!("lib")

      File.write!(filename, """
      defmodule Mix.Tasks.NimbleParsec.MinGtMaxTest.Parser do

        import NimbleParsec
        defparsec :parse, integer(min: 2, max: 1)
      end
      """)

      assert_raise ArgumentError,
                   "expected :max to be strictly more than :min, got: 2 and 1",
                   fn ->
                     Mix.Task.run("nimble_parsec.compile", [filename])
                   end
    end)
  end

  test "explicit min < 1" do
    filename = "lib/xmin_lt_one.ex.exs"

    in_tmp(fn _path ->
      File.mkdir_p!("lib")

      File.write!(filename, """
      defmodule Mix.Tasks.NimbleParsec.XminLtOneTest.Parser do

        import NimbleParsec
        defparsec :parse, integer(min: 0)
      end
      """)

      assert_raise ArgumentError,
                   "expected :min to be a positive integer, was 0",
                   fn ->
                     Mix.Task.run("nimble_parsec.compile", [filename])
                   end
    end)
  end

  test "implicit min < 1" do
    filename = "lib/imin_lt_one.ex.exs"

    in_tmp(fn _path ->
      File.mkdir_p!("lib")

      File.write!(filename, """
      defmodule Mix.Tasks.NimbleParsec.IminLtOneTest.Parser do

        import NimbleParsec
        defparsec :parse, integer(-1)
      end
      """)

      assert_raise ArgumentError,
                   "expected :min to be a positive integer, was -1",
                   fn ->
                     Mix.Task.run("nimble_parsec.compile", [filename])
                   end
    end)
  end
end
