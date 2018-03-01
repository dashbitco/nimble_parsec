defmodule NimbleParsecTest do
  use ExUnit.Case, async: true

  import NimbleParsec
  doctest NimbleParsec

  describe "integer/3 combinator with min=max" do
    defparsec :only_integer, integer(2, 2)
    defparsec :prefixed_integer, literal("T") |> integer(2, 2)

    test "returns ok/error alone" do
      assert only_integer("12") == {:ok, [12], "", 1, 3}
      assert only_integer("123") == {:ok, [12], "3", 1, 3}
      assert only_integer("1a3") == {:error, "1a3", 1, 1}
    end

    test "returns ok/error with previous document" do
      assert prefixed_integer("T12") == {:ok, ["T", 12], "", 1, 4}
      assert prefixed_integer("T123") == {:ok, ["T", 12], "3", 1, 4}
      assert prefixed_integer("T1a3") == {:error, "T1a3", 1, 1}
    end

    test "is bound" do
      assert bound?(integer(2, 2))
      assert bound?(literal("T") |> integer(2, 2) |> literal("E"))
    end
  end

  describe "datetime/2 combinator" do

  end

  defp bound?(document) do
    defs = NimbleParsec.Compiler.compile(:not_used, document, [])
    assert length(defs) == 3, "Expected #{inspect(document)} to contain 3 clauses, got #{length(defs)}"
  end
end
