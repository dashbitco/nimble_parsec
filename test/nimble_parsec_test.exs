defmodule NimbleParsecTest do
  use ExUnit.Case, async: true

  import NimbleParsec
  doctest NimbleParsec

  describe "integer/3 combinator with min=max" do
    defparsec :only_integer, integer(2, 2)
    defparsec :prefixed_integer, literal("T") |> integer(2, 2)

    test "returns ok/error by itself" do
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

  describe "literal/2 combinator" do
    defparsec :only_literal, literal("TO")
    defparsec :only_literal_with_newline, literal("T\nO")

    test "returns ok/error by itself" do
      assert only_literal("TO") == {:ok, ["TO"], "", 1, 3}
      assert only_literal("TOC") == {:ok, ["TO"], "C", 1, 3}
      assert only_literal("AO") == {:error, "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert only_literal_with_newline("T\nO") == {:ok, ["T\nO"], "", 2, 2}
      assert only_literal_with_newline("T\nOC") == {:ok, ["T\nO"], "C", 2, 2}
      assert only_literal_with_newline("A\nO") == {:error, "A\nO", 1, 1}
    end
  end

  describe "custom datetime/2 combinator" do
    datetime =
      integer(4, 4)
      |> ignore(literal("-"))
      |> integer(2, 2)
      |> ignore(literal("-"))
      |> integer(2, 2)
      |> ignore(literal("T"))
      |> integer(2, 2)
      |> ignore(literal(":"))
      |> integer(2, 2)
      |> ignore(literal(":"))
      |> integer(2, 2)

    defparsec(:datetime, datetime)

    test "returns ok/error by itself" do
      assert datetime("2010-04-17T14:12:34") == {:ok, [2010, 4, 17, 14, 12, 34], "", 1, 20}
    end
  end

  defp bound?(document) do
    defs = NimbleParsec.Compiler.compile(:not_used, document, [])
    assert length(defs) == 3, "Expected #{inspect(document)} to contain 3 clauses, got #{length(defs)}"
  end
end
