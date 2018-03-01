defmodule NimbleParsecTest do
  use ExUnit.Case, async: true

  import NimbleParsec
  doctest NimbleParsec

  describe "ascii_codepoint/2 combinator without newlines" do
    defparsec :only_ascii, ascii_codepoint([?0..?9]) |> ascii_codepoint([])
    defparsec :multi_ascii, ascii_codepoint([?0..?9, ?z..?a])
    defparsec :multi_ascii_with_not, ascii_codepoint([?0..?9, ?z..?a, not: ?c])
    defparsec :multi_ascii_with_multi_not, ascii_codepoint([?0..?9, ?z..?a, not: ?c, not: ?d..?e])
    defparsec :ascii_newline, ascii_codepoint([?0..?9, ?\n]) |> ascii_codepoint([?a..?z, ?\n])

    @error "expected byte in the range ?0..?9, followed by byte"

    test "returns ok/error on composition" do
      assert only_ascii("1a") == {:ok, [?1, ?a], "", 1, 3}
      assert only_ascii("11") == {:ok, [?1, ?1], "", 1, 3}
      assert only_ascii("a1") == {:error, @error, "a1", 1, 1}
    end

    @error "expected byte in the range ?0..?9 or in the range ?z..?a"

    test "returns ok/error on multiple ranges" do
      assert multi_ascii("1a") == {:ok, [?1], "a", 1, 2}
      assert multi_ascii("a1") == {:ok, [?a], "1", 1, 2}
      assert multi_ascii("++") == {:error, @error, "++", 1, 1}
    end

    @error "expected byte in the range ?0..?9 or in the range ?z..?a, and not equal to ?c"

    test "returns ok/error on multiple ranges with not" do
      assert multi_ascii_with_not("1a") == {:ok, [?1], "a", 1, 2}
      assert multi_ascii_with_not("a1") == {:ok, [?a], "1", 1, 2}
      assert multi_ascii_with_not("++") == {:error, @error, "++", 1, 1}
      assert multi_ascii_with_not("cc") == {:error, @error, "cc", 1, 1}
    end

    @error "expected byte in the range ?0..?9 or in the range ?z..?a, and not equal to ?c, and not in the range ?d..?e"

    test "returns ok/error on multiple ranges with multiple not" do
      assert multi_ascii_with_multi_not("1a") == {:ok, [?1], "a", 1, 2}
      assert multi_ascii_with_multi_not("a1") == {:ok, [?a], "1", 1, 2}
      assert multi_ascii_with_multi_not("++") == {:error, @error, "++", 1, 1}
      assert multi_ascii_with_multi_not("cc") == {:error, @error, "cc", 1, 1}
      assert multi_ascii_with_multi_not("de") == {:error, @error, "de", 1, 1}
    end

    test "returns ok/error even with newlines" do
      assert ascii_newline("1a\n") == {:ok, [?1, ?a], "\n", 1, 3}
      assert ascii_newline("1\na") == {:ok, [?1, ?\n], "a", 2, 1}
      assert ascii_newline("\nao") == {:ok, [?\n, ?a], "o", 2, 2}
    end

    test "is bound" do
      assert bound?(ascii_codepoint([?0..?9]))
      assert bound?(ascii_codepoint(not: ?\n))
    end
  end

  describe "utf8_codepoint/2 combinator without newlines" do
    defparsec :only_utf8, utf8_codepoint([?0..?9]) |> utf8_codepoint([])
    defparsec :utf8_newline, utf8_codepoint([]) |> utf8_codepoint([?a..?z, ?\n])

    @error "expected utf8 codepoint in the range ?0..?9, followed by utf8 codepoint"

    test "returns ok/error on composition" do
      assert only_utf8("1a") == {:ok, [?1, ?a], "", 1, 3}
      assert only_utf8("11") == {:ok, [?1, ?1], "", 1, 3}
      assert only_utf8("1é") == {:ok, [?1, ?é], "", 1, 3}
      assert only_utf8("a1") == {:error, @error, "a1", 1, 1}
    end

    test "returns ok/error even with newlines" do
      assert utf8_newline("1a\n") == {:ok, [?1, ?a], "\n", 1, 3}
      assert utf8_newline("1\na") == {:ok, [?1, ?\n], "a", 2, 1}
      assert utf8_newline("éa\n") == {:ok, [?é, ?a], "\n", 1, 3}
      assert utf8_newline("é\na") == {:ok, [?é, ?\n], "a", 2, 1}
      assert utf8_newline("\nao") == {:ok, [?\n, ?a], "o", 2, 2}
    end

    test "is bound" do
      assert bound?(utf8_codepoint([?0..?9]))
      assert bound?(utf8_codepoint(not: ?\n))
    end
  end

  describe "integer/3 combinator with min=max" do
    defparsec :only_integer, integer(2, 2)
    defparsec :prefixed_integer, literal("T") |> integer(2, 2)

    @error "expected 2 digits integer"

    test "returns ok/error by itself" do
      assert only_integer("12") == {:ok, [12], "", 1, 3}
      assert only_integer("123") == {:ok, [12], "3", 1, 3}
      assert only_integer("1a3") == {:error, @error, "1a3", 1, 1}
    end

    @error "expected literal \"T\", followed by 2 digits integer"

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
    defparsec :only_literal, literal("TO")
    defparsec :only_literal_with_newline, literal("T\nO")

    test "returns ok/error" do
      assert only_literal("TO") == {:ok, ["TO"], "", 1, 3}
      assert only_literal("TOC") == {:ok, ["TO"], "C", 1, 3}
      assert only_literal("AO") == {:error, "expected literal \"TO\"", "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert only_literal_with_newline("T\nO") == {:ok, ["T\nO"], "", 2, 2}
      assert only_literal_with_newline("T\nOC") == {:ok, ["T\nO"], "C", 2, 2}

      assert only_literal_with_newline("A\nO") ==
               {:error, "expected literal \"T\\nO\"", "A\nO", 1, 1}
    end

    test "is bound" do
      assert bound?(literal("T"))
    end
  end

  describe "ignore/2 combinator at compile time" do
    defparsec :compile_ignore, ignore(literal("TO"))
    defparsec :compile_ignore_with_newline, ignore(literal("T\nO"))

    test "returns ok/error" do
      assert compile_ignore("TO") == {:ok, [], "", 1, 3}
      assert compile_ignore("TOC") == {:ok, [], "C", 1, 3}
      assert compile_ignore("AO") == {:error, "expected literal \"TO\"", "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert compile_ignore_with_newline("T\nO") == {:ok, [], "", 2, 2}
      assert compile_ignore_with_newline("T\nOC") == {:ok, [], "C", 2, 2}

      assert compile_ignore_with_newline("A\nO") ==
               {:error, "expected literal \"T\\nO\"", "A\nO", 1, 1}
    end

    test "is bound" do
      assert bound?(ignore(literal("T")))
    end
  end

  describe "ignore/2 combinator at runtime" do
    defparsec :runtime_ignore,
              ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> map({:to_string, []})
              |> ignore()

    test "returns ok/error" do
      assert runtime_ignore("abc") == {:ok, [], "", 1, 4}

      error =
        "expected byte in the range ?a..?z, followed by byte in the range ?a..?z, followed by byte in the range ?a..?z"

      assert runtime_ignore("1bc") == {:error, error, "1bc", 1, 1}
    end

    test "is not bound" do
      assert not_bound?(ascii_codepoint([?a..?z]) |> map({:to_string, []}) |> ignore())
    end
  end

  describe "replace/3 combinator at compile time" do
    defparsec :compile_replace, replace(literal("TO"), "OTHER")
    defparsec :compile_replace_with_newline, replace(literal("T\nO"), "OTHER")

    test "returns ok/error" do
      assert compile_replace("TO") == {:ok, ["OTHER"], "", 1, 3}
      assert compile_replace("TOC") == {:ok, ["OTHER"], "C", 1, 3}
      assert compile_replace("AO") == {:error, "expected literal \"TO\"", "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert compile_replace_with_newline("T\nO") == {:ok, ["OTHER"], "", 2, 2}
      assert compile_replace_with_newline("T\nOC") == {:ok, ["OTHER"], "C", 2, 2}

      assert compile_replace_with_newline("A\nO") ==
               {:error, "expected literal \"T\\nO\"", "A\nO", 1, 1}
    end

    test "is bound" do
      assert bound?(replace(literal("T"), "OTHER"))
    end
  end

  describe "replace/2 combinator at runtime" do
    defparsec :runtime_replace,
              ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> map({:to_string, []})
              |> replace("OTHER")

    test "returns ok/error" do
      assert runtime_replace("abc") == {:ok, ["OTHER"], "", 1, 4}

      error =
        "expected byte in the range ?a..?z, followed by byte in the range ?a..?z, followed by byte in the range ?a..?z"

      assert runtime_replace("1bc") == {:error, error, "1bc", 1, 1}
    end

    test "is not bound" do
      assert not_bound?(ascii_codepoint([?a..?z]) |> map({:to_string, []}) |> replace("OTHER"))
    end
  end

  describe "label/3 combinator at compile time" do
    defparsec :compile_label, label(literal("TO"), "label")
    defparsec :compile_label_with_newline, label(literal("T\nO"), "label")

    test "returns ok/error" do
      assert compile_label("TO") == {:ok, ["TO"], "", 1, 3}
      assert compile_label("TOC") == {:ok, ["TO"], "C", 1, 3}
      assert compile_label("AO") == {:error, "expected label", "AO", 1, 1}
    end

    test "properly counts newlines" do
      assert compile_label_with_newline("T\nO") == {:ok, ["T\nO"], "", 2, 2}
      assert compile_label_with_newline("T\nOC") == {:ok, ["T\nO"], "C", 2, 2}
      assert compile_label_with_newline("A\nO") == {:error, "expected label", "A\nO", 1, 1}
    end

    test "is bound" do
      assert bound?(label(literal("T"), "label"))
    end
  end

  describe "label/3 combinator at runtime" do
    defparsec :runtime_label,
              label(ascii_codepoint([?a..?z]), "first label")
              |> label(ascii_codepoint([?a..?z]) |> map({:to_string, []}), "second label")
              |> ascii_codepoint([?a..?z])
              |> map({:to_string, []})
              |> label("third label")

    test "returns ok/error" do
      assert runtime_label("abc") == {:ok, ["97", "98", "99"], "", 1, 4}

      error = "expected third label"
      assert runtime_label("1bc") == {:error, error, "1bc", 1, 1}

      error = "expected second label while processing third label"
      assert runtime_label("a1c") == {:error, error, "1c", 1, 2}

      error = "expected third label"
      assert runtime_label("ab1") == {:error, error, "1", 1, 3}
    end

    test "is not bound" do
      assert not_bound?(ascii_codepoint([?a..?z]) |> map({:to_string, []}) |> label("label"))
    end
  end

  describe "remote traverse/3 combinator" do
    @three_ascii_letters ascii_codepoint([?a..?z])
                         |> ascii_codepoint([?a..?z])
                         |> ascii_codepoint([?a..?z])

    defparsec :remote_traverse,
              literal("T")
              |> integer(2, 2)
              |> traverse(@three_ascii_letters, {__MODULE__, :public_join_and_wrap, ["-"]})
              |> integer(2, 2)

    test "returns ok/error" do
      assert remote_traverse("T12abc34") == {:ok, ["T", 12, "99-98-97", 34], "", 1, 9}

      error = "expected literal \"T\", followed by 2 digits integer"
      assert remote_traverse("Tabc34") == {:error, error, "Tabc34", 1, 1}

      error = "expected 2 digits integer"
      assert remote_traverse("T12abcdf") == {:error, error, "df", 1, 7}

      error =
        "expected byte in the range ?a..?z, followed by byte in the range ?a..?z, followed by byte in the range ?a..?z"

      assert remote_traverse("T12ab34") == {:error, error, "ab34", 1, 4}
    end

    test "is not bound" do
      assert not_bound?(
               traverse(@three_ascii_letters, {__MODULE__, :public_join_and_wrap, ["-"]})
             )
    end

    def public_join_and_wrap(args, joiner) do
      args |> Enum.join(joiner) |> List.wrap()
    end
  end

  describe "local traverse/3 combinator" do
    @three_ascii_letters ascii_codepoint([?a..?z])
                         |> ascii_codepoint([?a..?z])
                         |> ascii_codepoint([?a..?z])

    defparsec :local_traverse,
              literal("T")
              |> integer(2, 2)
              |> traverse(@three_ascii_letters, {:private_join_and_wrap, ["-"]})
              |> integer(2, 2)

    test "returns ok/error" do
      assert local_traverse("T12abc34") == {:ok, ["T", 12, "99-98-97", 34], "", 1, 9}

      error = "expected literal \"T\", followed by 2 digits integer"
      assert local_traverse("Tabc34") == {:error, error, "Tabc34", 1, 1}

      error = "expected 2 digits integer"
      assert local_traverse("T12abcdf") == {:error, error, "df", 1, 7}

      error =
        "expected byte in the range ?a..?z, followed by byte in the range ?a..?z, followed by byte in the range ?a..?z"

      assert local_traverse("T12ab34") == {:error, error, "ab34", 1, 4}
    end

    test "is not bound" do
      assert not_bound?(traverse(@three_ascii_letters, {:public_join_and_wrap, ["-"]}))
    end

    defp private_join_and_wrap(args, joiner) do
      args |> Enum.join(joiner) |> List.wrap()
    end
  end

  describe "remote map/3 combinator" do
    defparsec :remote_map,
              ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> map({Integer, :to_string, []})

    test "returns ok/error" do
      assert remote_map("abc") == {:ok, ["97", "98", "99"], "", 1, 4}
      assert remote_map("abcd") == {:ok, ["97", "98", "99"], "d", 1, 4}
      assert {:error, _, "1abcd", 1, 1} = remote_map("1abcd")
    end
  end

  describe "local map/3 combinator" do
    defparsec :local_map,
              ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> map({:local_to_string, []})

    test "returns ok/error" do
      assert local_map("abc") == {:ok, ["97", "98", "99"], "", 1, 4}
      assert local_map("abcd") == {:ok, ["97", "98", "99"], "d", 1, 4}
      assert {:error, _, "1abcd", 1, 1} = local_map("1abcd")
    end

    defp local_to_string(arg) do
      Integer.to_string(arg)
    end
  end

  describe "remote reduce/3 combinator" do
    defparsec :remote_reduce,
              ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> reduce({Enum, :join, ["-"]})

    test "returns ok/error" do
      assert remote_reduce("abc") == {:ok, ["97-98-99"], "", 1, 4}
      assert remote_reduce("abcd") == {:ok, ["97-98-99"], "d", 1, 4}
      assert {:error, _, "1abcd", 1, 1} = remote_reduce("1abcd")
    end
  end

  describe "local reduce/3 combinator" do
    defparsec :local_reduce,
              ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> ascii_codepoint([?a..?z])
              |> reduce({:local_join, ["-"]})

    test "returns ok/error" do
      assert local_reduce("abc") == {:ok, ["97-98-99"], "", 1, 4}
      assert local_reduce("abcd") == {:ok, ["97-98-99"], "d", 1, 4}
      assert {:error, _, "1abcd", 1, 1} = local_reduce("1abcd")
    end

    defp local_join(list, joiner) do
      Enum.join(list, joiner)
    end
  end

  describe "concat/2 combinator" do
    defparsec :concat_digit_upper_lower_plus,
              concat(
                concat(ascii_codepoint([?0..?9]), ascii_codepoint([?A..?Z])),
                concat(ascii_codepoint([?a..?z]), ascii_codepoint([?+..?+]))
              )

    test "returns ok/error" do
      assert concat_digit_upper_lower_plus("1Az+") == {:ok, [?1, ?A, ?z, ?+], "", 1, 5}
    end
  end

  describe "custom datetime/2 combinator" do
    date =
      integer(4, 4)
      |> ignore(literal("-"))
      |> integer(2, 2)
      |> ignore(literal("-"))
      |> integer(2, 2)

    time =
      integer(2, 2)
      |> ignore(literal(":"))
      |> integer(2, 2)
      |> ignore(literal(":"))
      |> integer(2, 2)

    defparsec :datetime, date |> ignore(literal("T")) |> concat(time)

    test "returns ok/error by itself" do
      assert datetime("2010-04-17T14:12:34") == {:ok, [2010, 4, 17, 14, 12, 34], "", 1, 20}
    end
  end

  defp bound?(document) do
    {defs, _} = NimbleParsec.Compiler.compile(:not_used, document, [])

    assert length(defs) == 3,
           "Expected #{inspect(document)} to contain 3 clauses, got #{length(defs)}"
  end

  defp not_bound?(document) do
    {defs, _} = NimbleParsec.Compiler.compile(:not_used, document, [])

    assert length(defs) != 3, "Expected #{inspect(document)} to contain more than 3 clauses"
  end
end
