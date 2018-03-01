defmodule NimbleParsecTest do
  use ExUnit.Case, async: true

  import NimbleParsec
  doctest NimbleParsec

  describe "ascii_codepoint/2 combinator without newlines" do
    defparsec(:only_ascii_codepoints, ascii_codepoint([?0..?9]) |> ascii_codepoint([?z..?a]))

    @error "expected a byte in the range ?0..?9, followed by a byte in the range ?z..?a"

    test "returns ok/error" do
      assert only_ascii_codepoints("1a") == {:ok, [?1, ?a], "", 1, 3}
      assert only_ascii_codepoints("a1") == {:error, @error, "a1", 1, 1}
      assert only_ascii_codepoints("11") == {:error, @error, "11", 1, 1}
    end

    test "is bound" do
      assert bound?(ascii_codepoint([?0..?9]) |> ascii_codepoint([?a..?z]))
    end
  end

  describe "integer/3 combinator with min=max" do
    defparsec(:only_integer, integer(2, 2))
    defparsec(:prefixed_integer, literal("T") |> integer(2, 2))

    @error "expected a 2 digits integer"

    test "returns ok/error by itself" do
      assert only_integer("12") == {:ok, [12], "", 1, 3}
      assert only_integer("123") == {:ok, [12], "3", 1, 3}
      assert only_integer("1a3") == {:error, @error, "1a3", 1, 1}
    end

    @error "expected a literal \"T\", followed by a 2 digits integer"

    test "returns ok/error with previous document" do
      assert prefixed_integer("T12") == {:ok, ["T", 12], "", 1, 4}
      assert prefixed_integer("T123") == {:ok, ["T", 12], "3", 1, 4}
      assert prefixed_integer("T1a3") == {:error, @error, "T1a3", 1, 1}
    end

    test "is bound" do
      assert bound?(integer(2, 2))
      assert bound?(literal("T") |> integer(2, 2))
      assert bound?(literal("T") |> integer(2, 2) |> literal("E"))
    end
  end

  describe "literal/2 combinator" do
    defparsec(:only_literal, literal("TO"))
    defparsec(:only_literal_with_newline, literal("T\nO"))

    test "returns ok/error" do
      assert only_literal("TO") == {:ok, ["TO"], "", 1, 3}
      assert only_literal("TOC") == {:ok, ["TO"], "C", 1, 3}
      assert only_literal("AO") == {:error, "expected a literal \"TO\"", "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert only_literal_with_newline("T\nO") == {:ok, ["T\nO"], "", 2, 2}
      assert only_literal_with_newline("T\nOC") == {:ok, ["T\nO"], "C", 2, 2}

      assert only_literal_with_newline("A\nO") ==
               {:error, "expected a literal \"T\\nO\"", "A\nO", 1, 1}
    end

    test "is bound" do
      assert bound?(literal("T"))
    end
  end

  describe "ignore/2 combinator at compile time" do
    defparsec(:only_ignore, ignore(literal("TO")))
    defparsec(:only_ignore_with_newline, ignore(literal("T\nO")))

    test "returns ok/error" do
      assert only_ignore("TO") == {:ok, [], "", 1, 3}
      assert only_ignore("TOC") == {:ok, [], "C", 1, 3}
      assert only_ignore("AO") == {:error, "expected a literal \"TO\"", "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert only_ignore_with_newline("T\nO") == {:ok, [], "", 2, 2}
      assert only_ignore_with_newline("T\nOC") == {:ok, [], "C", 2, 2}

      assert only_ignore_with_newline("A\nO") ==
               {:error, "expected a literal \"T\\nO\"", "A\nO", 1, 1}
    end

    test "is bound" do
      assert bound?(ignore(literal("T")))
    end
  end

  describe "label/2 combinator at compile time" do
    defparsec(:only_label, label(literal("TO"), "a label"))
    defparsec(:only_label_with_newline, label(literal("T\nO"), "a label"))

    test "returns ok/error" do
      assert only_label("TO") == {:ok, ["TO"], "", 1, 3}
      assert only_label("TOC") == {:ok, ["TO"], "C", 1, 3}
      assert only_label("AO") == {:error, "expected a label", "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert only_label_with_newline("T\nO") == {:ok, ["T\nO"], "", 2, 2}
      assert only_label_with_newline("T\nOC") == {:ok, ["T\nO"], "C", 2, 2}

      assert only_label_with_newline("A\nO") ==
               {:error, "expected a label", "A\nO", 1, 1}
    end

    test "is bound" do
      assert bound?(label(literal("T"), "a label"))
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

    assert length(defs) == 3,
           "Expected #{inspect(document)} to contain 3 clauses, got #{length(defs)}"
  end
end
