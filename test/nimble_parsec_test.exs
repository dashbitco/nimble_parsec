defmodule NimbleParsecTest do
  use ExUnit.Case, async: true

  import NimbleParsec
  import ExUnit.CaptureIO

  describe "ascii_char/2 combinator without newlines" do
    defparsecp :only_ascii, ascii_char([?0..?9]) |> ascii_char([])
    defparsecp :multi_ascii, ascii_char([?0..?9, ?z..?a])
    defparsecp :multi_ascii_with_not, ascii_char([?0..?9, ?z..?a, not: ?c])
    defparsecp :multi_ascii_with_multi_not, ascii_char([?0..?9, ?z..?a, not: ?c, not: ?d..?e])
    defparsecp :ascii_newline, ascii_char([?0..?9, ?\n]) |> ascii_char([?a..?z, ?\n])
    defparsecp :ascii_only_newline, ascii_char([?\n])
    defparsecp :none_ascii, ascii_char([?\a..?\n])

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character"

    test "returns ok/error on composition" do
      assert only_ascii("1a") == {:ok, [?1, ?a], "", %{}, {1, 0}, 2}
      assert only_ascii("11") == {:ok, [?1, ?1], "", %{}, {1, 0}, 2}
      assert only_ascii("a1") == {:error, @error, "a1", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character in the range '0' to '9' or in the range 'z' to 'a'"

    test "returns ok/error on multiple ranges" do
      assert multi_ascii("1a") == {:ok, [?1], "a", %{}, {1, 0}, 1}
      assert multi_ascii("a1") == {:ok, [?a], "1", %{}, {1, 0}, 1}
      assert multi_ascii("++") == {:error, @error, "++", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character in the range '0' to '9' or in the range 'z' to 'a', and not equal to 'c'"

    test "returns ok/error on multiple ranges with not" do
      assert multi_ascii_with_not("1a") == {:ok, [?1], "a", %{}, {1, 0}, 1}
      assert multi_ascii_with_not("a1") == {:ok, [?a], "1", %{}, {1, 0}, 1}
      assert multi_ascii_with_not("++") == {:error, @error, "++", %{}, {1, 0}, 0}
      assert multi_ascii_with_not("cc") == {:error, @error, "cc", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character in the range '0' to '9' or in the range 'z' to 'a', and not equal to 'c', and not in the range 'd' to 'e'"

    test "returns ok/error on multiple ranges with multiple not" do
      assert multi_ascii_with_multi_not("1a") == {:ok, [?1], "a", %{}, {1, 0}, 1}
      assert multi_ascii_with_multi_not("a1") == {:ok, [?a], "1", %{}, {1, 0}, 1}
      assert multi_ascii_with_multi_not("++") == {:error, @error, "++", %{}, {1, 0}, 0}
      assert multi_ascii_with_multi_not("cc") == {:error, @error, "cc", %{}, {1, 0}, 0}
      assert multi_ascii_with_multi_not("de") == {:error, @error, "de", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character in the range '0' to '9' or equal to '\\n', followed by ASCII character in the range 'a' to 'z' or equal to '\\n'"

    test "returns ok/error even with newlines" do
      assert ascii_newline("1a\n") == {:ok, [?1, ?a], "\n", %{}, {1, 0}, 2}
      assert ascii_newline("1\na") == {:ok, [?1, ?\n], "a", %{}, {2, 2}, 2}
      assert ascii_newline("\nao") == {:ok, [?\n, ?a], "o", %{}, {2, 1}, 2}
      assert ascii_newline("x") == {:error, @error, "x", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character equal to '\\n'"

    test "returns ok/error on only newline" do
      assert ascii_only_newline("\n") == {:ok, '\n', "", %{}, {2, 1}, 1}
      assert ascii_only_newline("x") == {:error, @error, "x", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character in the range '\\a' to '\\n'"

    test "returns ok/error on none ascii range" do
      assert none_ascii("\a\t\n") == {:ok, '\a', "\t\n", %{}, {1, 0}, 1}
      assert none_ascii("x") == {:error, @error, "x", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(ascii_char([?0..?9]))
      assert bound?(ascii_char(not: ?\n))
    end
  end

  describe "utf8_char/2 combinator without newlines" do
    defparsecp :only_utf8, utf8_char([?0..?9]) |> utf8_char([])
    defparsecp :utf8_newline, utf8_char([]) |> utf8_char([?a..?z, ?\n])

    @error "expected utf8 codepoint in the range '0' to '9', followed by utf8 codepoint"

    test "returns ok/error on composition" do
      assert only_utf8("1a") == {:ok, [?1, ?a], "", %{}, {1, 0}, 2}
      assert only_utf8("11") == {:ok, [?1, ?1], "", %{}, {1, 0}, 2}
      assert only_utf8("1é") == {:ok, [?1, ?é], "", %{}, {1, 0}, 3}
      assert only_utf8("a1") == {:error, @error, "a1", %{}, {1, 0}, 0}
    end

    test "returns ok/error even with newlines" do
      assert utf8_newline("1a\n") == {:ok, [?1, ?a], "\n", %{}, {1, 0}, 2}
      assert utf8_newline("1\na") == {:ok, [?1, ?\n], "a", %{}, {2, 2}, 2}
      assert utf8_newline("éa\n") == {:ok, [?é, ?a], "\n", %{}, {1, 0}, 3}
      assert utf8_newline("é\na") == {:ok, [?é, ?\n], "a", %{}, {2, 3}, 3}
      assert utf8_newline("\nao") == {:ok, [?\n, ?a], "o", %{}, {2, 1}, 2}
    end

    test "is bound" do
      assert bound?(utf8_char([?0..?9]))
      assert bound?(utf8_char(not: ?\n))
    end
  end

  describe "integer/2 combinator with exact length" do
    defparsecp :exact_integer, integer(2)
    defparsecp :prefixed_integer, string("T") |> integer(2)

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

    test "returns ok/error" do
      assert exact_integer("12") == {:ok, [12], "", %{}, {1, 0}, 2}
      assert exact_integer("123") == {:ok, [12], "3", %{}, {1, 0}, 2}
      assert exact_integer("1a3") == {:error, @error, "1a3", %{}, {1, 0}, 0}
    end

    @error "expected string \"T\", followed by ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

    test "returns ok/error with previous document" do
      assert prefixed_integer("T12") == {:ok, ["T", 12], "", %{}, {1, 0}, 3}
      assert prefixed_integer("T123") == {:ok, ["T", 12], "3", %{}, {1, 0}, 3}
      assert prefixed_integer("T1a3") == {:error, @error, "T1a3", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(integer(2))
      assert bound?(string("T") |> integer(2))
      assert bound?(string("T") |> integer(2) |> string("E"))
    end
  end

  describe "integer/2 combinator with min/max" do
    defparsecp :min_integer, integer(min: 2)
    defparsecp :max_integer, integer(max: 3)
    defparsecp :min_max_integer, integer(min: 2, max: 3)

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

    test "returns ok/error with min" do
      assert min_integer("12") == {:ok, [12], "", %{}, {1, 0}, 2}
      assert min_integer("123") == {:ok, [123], "", %{}, {1, 0}, 3}
      assert min_integer("123o") == {:ok, [123], "o", %{}, {1, 0}, 3}
      assert min_integer("1234") == {:ok, [1234], "", %{}, {1, 0}, 4}
      assert min_integer("1") == {:error, @error, "1", %{}, {1, 0}, 0}
    end

    test "returns ok/error with max" do
      assert max_integer("1") == {:ok, [1], "", %{}, {1, 0}, 1}
      assert max_integer("12") == {:ok, [12], "", %{}, {1, 0}, 2}
      assert max_integer("123") == {:ok, [123], "", %{}, {1, 0}, 3}
      assert max_integer("1234") == {:ok, [123], "4", %{}, {1, 0}, 3}
      assert max_integer("123o") == {:ok, [123], "o", %{}, {1, 0}, 3}
    end

    test "returns ok/error with min/max" do
      assert min_max_integer("1") == {:error, @error, "1", %{}, {1, 0}, 0}
      assert min_max_integer("12") == {:ok, [12], "", %{}, {1, 0}, 2}
      assert min_max_integer("123") == {:ok, [123], "", %{}, {1, 0}, 3}
      assert min_max_integer("1234") == {:ok, [123], "4", %{}, {1, 0}, 3}
      assert min_max_integer("123o") == {:ok, [123], "o", %{}, {1, 0}, 3}
    end

    test "is not bound" do
      assert not_bound?(integer(min: 3))
      assert not_bound?(integer(max: 3))
      assert not_bound?(integer(min: 1, max: 3))
    end
  end

  describe "string/2 combinator" do
    defparsecp :only_string, string("TO")
    defparsecp :only_string_with_newline, string("T\nO")

    test "returns ok/error" do
      assert only_string("TO") == {:ok, ["TO"], "", %{}, {1, 0}, 2}
      assert only_string("TOC") == {:ok, ["TO"], "C", %{}, {1, 0}, 2}
      assert only_string("AO") == {:error, "expected string \"TO\"", "AO", %{}, {1, 0}, 0}
    end

    test "properly counts newlines" do
      assert only_string_with_newline("T\nO") == {:ok, ["T\nO"], "", %{}, {2, 2}, 3}
      assert only_string_with_newline("T\nOC") == {:ok, ["T\nO"], "C", %{}, {2, 2}, 3}

      assert only_string_with_newline("A\nO") ==
               {:error, "expected string \"T\\nO\"", "A\nO", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(string("T"))
    end
  end

  describe "ascii_string/2 combinator with exact length" do
    defparsecp :exact_ascii_string, ascii_string([?a..?z], 2)

    @error "expected ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z'"

    test "returns ok/error" do
      assert exact_ascii_string("ab") == {:ok, ["ab"], "", %{}, {1, 0}, 2}
      assert exact_ascii_string("abc") == {:ok, ["ab"], "c", %{}, {1, 0}, 2}
      assert exact_ascii_string("1ab") == {:error, @error, "1ab", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(ascii_string([?a..?z], 2))
    end
  end

  describe "ascii_string/2 combinator with min/max" do
    defparsecp :min_ascii_string, ascii_string([?0..?9], min: 2)
    defparsecp :max_ascii_string, ascii_string([?0..?9], max: 3)
    defparsecp :min_max_ascii_string, ascii_string([?0..?9], min: 2, max: 3)

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

    test "returns ok/error with min" do
      assert min_ascii_string("12") == {:ok, ["12"], "", %{}, {1, 0}, 2}
      assert min_ascii_string("123") == {:ok, ["123"], "", %{}, {1, 0}, 3}
      assert min_ascii_string("123o") == {:ok, ["123"], "o", %{}, {1, 0}, 3}
      assert min_ascii_string("1234") == {:ok, ["1234"], "", %{}, {1, 0}, 4}
      assert min_ascii_string("1") == {:error, @error, "1", %{}, {1, 0}, 0}
    end

    test "returns ok/error with max" do
      assert max_ascii_string("1") == {:ok, ["1"], "", %{}, {1, 0}, 1}
      assert max_ascii_string("12") == {:ok, ["12"], "", %{}, {1, 0}, 2}
      assert max_ascii_string("123") == {:ok, ["123"], "", %{}, {1, 0}, 3}
      assert max_ascii_string("1234") == {:ok, ["123"], "4", %{}, {1, 0}, 3}
      assert max_ascii_string("123o") == {:ok, ["123"], "o", %{}, {1, 0}, 3}
    end

    test "returns ok/error with min/max" do
      assert min_max_ascii_string("1") == {:error, @error, "1", %{}, {1, 0}, 0}
      assert min_max_ascii_string("12") == {:ok, ["12"], "", %{}, {1, 0}, 2}
      assert min_max_ascii_string("123") == {:ok, ["123"], "", %{}, {1, 0}, 3}
      assert min_max_ascii_string("1234") == {:ok, ["123"], "4", %{}, {1, 0}, 3}
      assert min_max_ascii_string("12o") == {:ok, ["12"], "o", %{}, {1, 0}, 2}
    end

    test "is not bound" do
      assert not_bound?(ascii_string([?0..?9], min: 3))
      assert not_bound?(ascii_string([?0..?9], max: 3))
      assert not_bound?(ascii_string([?0..?9], min: 1, max: 3))
    end
  end

  describe "utf8_string/2 combinator with exact length" do
    defparsecp :exact_utf8_string, utf8_string([], 2)
    defparsecp :zero_utf8_string, utf8_string([], 0)

    @error "expected utf8 codepoint, followed by utf8 codepoint"

    test "returns ok/error" do
      assert exact_utf8_string("áé") == {:ok, ["áé"], "", %{}, {1, 0}, 4}
      assert exact_utf8_string("áé\xFF") == {:ok, ["áé"], "\xFF", %{}, {1, 0}, 4}
      assert exact_utf8_string("\xFFáé") == {:error, @error, "\xFFáé", %{}, {1, 0}, 0}

      assert zero_utf8_string("áé") == {:ok, [""], "áé", %{}, {1, 0}, 0}
      assert zero_utf8_string("") == {:ok, [""], "", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(utf8_string([], 2))
    end
  end

  describe "utf8_string/2 combinator with min/max" do
    defparsecp :min_utf8_string, utf8_string([], min: 2)
    defparsecp :max_utf8_string, utf8_string([], max: 3)
    defparsecp :min_max_utf8_string, utf8_string([], min: 2, max: 3)
    defparsecp :min_zero_utf8_string, utf8_string([], min: 0)

    @error "expected utf8 codepoint, followed by utf8 codepoint"

    test "returns ok/error with min" do
      assert min_utf8_string("áé") == {:ok, ["áé"], "", %{}, {1, 0}, 4}
      assert min_utf8_string("áéí") == {:ok, ["áéí"], "", %{}, {1, 0}, 6}
      assert min_utf8_string("áéí\xFF") == {:ok, ["áéí"], "\xFF", %{}, {1, 0}, 6}
      assert min_utf8_string("áéíó") == {:ok, ["áéíó"], "", %{}, {1, 0}, 8}
      assert min_utf8_string("\xFF") == {:error, @error, "\xFF", %{}, {1, 0}, 0}

      assert min_zero_utf8_string("áé") == {:ok, ["áé"], "", %{}, {1, 0}, 4}
      assert min_zero_utf8_string("\xFF") == {:ok, [""], "\xFF", %{}, {1, 0}, 0}
    end

    test "returns ok/error with max" do
      assert max_utf8_string("á") == {:ok, ["á"], "", %{}, {1, 0}, 2}
      assert max_utf8_string("áé") == {:ok, ["áé"], "", %{}, {1, 0}, 4}
      assert max_utf8_string("áéí") == {:ok, ["áéí"], "", %{}, {1, 0}, 6}
      assert max_utf8_string("áéíó") == {:ok, ["áéí"], "ó", %{}, {1, 0}, 6}
      assert max_utf8_string("áéí\xFF") == {:ok, ["áéí"], "\xFF", %{}, {1, 0}, 6}
    end

    test "returns ok/error with min/max" do
      assert min_max_utf8_string("á") == {:error, @error, "á", %{}, {1, 0}, 0}
      assert min_max_utf8_string("áé") == {:ok, ["áé"], "", %{}, {1, 0}, 4}
      assert min_max_utf8_string("áéí") == {:ok, ["áéí"], "", %{}, {1, 0}, 6}
      assert min_max_utf8_string("áéíó") == {:ok, ["áéí"], "ó", %{}, {1, 0}, 6}
      assert min_max_utf8_string("áé\xFF") == {:ok, ["áé"], "\xFF", %{}, {1, 0}, 4}
    end

    test "is not bound" do
      assert not_bound?(utf8_string([], min: 3))
      assert not_bound?(utf8_string([], max: 3))
      assert not_bound?(utf8_string([], min: 1, max: 3))
    end
  end

  describe "ignore/2 combinator at compile time" do
    defparsecp :compile_ignore, ignore(string("TO"))
    defparsecp :compile_ignore_with_newline, ignore(string("T\nO"))

    test "returns ok/error" do
      assert compile_ignore("TO") == {:ok, [], "", %{}, {1, 0}, 2}
      assert compile_ignore("TOC") == {:ok, [], "C", %{}, {1, 0}, 2}
      assert compile_ignore("AO") == {:error, "expected string \"TO\"", "AO", %{}, {1, 0}, 0}
    end

    test "properly counts newlines" do
      assert compile_ignore_with_newline("T\nO") == {:ok, [], "", %{}, {2, 2}, 3}
      assert compile_ignore_with_newline("T\nOC") == {:ok, [], "C", %{}, {2, 2}, 3}

      assert compile_ignore_with_newline("A\nO") ==
               {:error, "expected string \"T\\nO\"", "A\nO", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(ignore(string("T")))
    end
  end

  describe "ignore/2 combinator at runtime" do
    defparsecp :runtime_ignore,
               ascii_char([?a..?z])
               |> times(min: 1)
               |> ignore()

    defparsecp :ignore_one,
               ascii_char([])
               |> times(1)
               |> ignore

    test "returns ok/error" do
      assert runtime_ignore("abc") == {:ok, [], "", %{}, {1, 0}, 3}
      error = "expected ASCII character in the range 'a' to 'z'"
      assert runtime_ignore("1bc") == {:error, error, "1bc", %{}, {1, 0}, 0}
    end

    test "ignore one" do
      assert ignore_one("abc") == {:ok, [], "bc", %{}, {1, 0}, 1}
      error = "expected ASCII character"
      assert ignore_one("") == {:error, error, "", %{}, {1, 0}, 0}
    end

    test "is not bound" do
      assert not_bound?(ascii_char([?a..?z]) |> times(min: 1) |> ignore())
    end
  end

  describe "replace/3 combinator at compile time" do
    defparsecp :compile_replace, replace(string("TO"), "OTHER")
    defparsecp :compile_replace_with_newline, replace(string("T\nO"), "OTHER")
    defparsecp :compile_replace_empty, replace(empty(), "OTHER")

    test "returns ok/error" do
      assert compile_replace("TO") == {:ok, ["OTHER"], "", %{}, {1, 0}, 2}
      assert compile_replace("TOC") == {:ok, ["OTHER"], "C", %{}, {1, 0}, 2}
      assert compile_replace("AO") == {:error, "expected string \"TO\"", "AO", %{}, {1, 0}, 0}
    end

    test "can replace empty" do
      assert compile_replace_empty("TO") == {:ok, ["OTHER"], "TO", %{}, {1, 0}, 0}
    end

    test "properly counts newlines" do
      assert compile_replace_with_newline("T\nO") == {:ok, ["OTHER"], "", %{}, {2, 2}, 3}
      assert compile_replace_with_newline("T\nOC") == {:ok, ["OTHER"], "C", %{}, {2, 2}, 3}

      assert compile_replace_with_newline("A\nO") ==
               {:error, "expected string \"T\\nO\"", "A\nO", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(replace(string("T"), "OTHER"))
      assert bound?(replace(empty(), "OTHER"))
    end
  end

  describe "replace/2 combinator at runtime" do
    defparsecp :runtime_replace,
               ascii_char([?a..?z])
               |> times(min: 1)
               |> replace("OTHER")

    test "returns ok/error" do
      assert runtime_replace("abc") == {:ok, ["OTHER"], "", %{}, {1, 0}, 3}
      error = "expected ASCII character in the range 'a' to 'z'"
      assert runtime_replace("1bc") == {:error, error, "1bc", %{}, {1, 0}, 0}
    end

    test "is not bound" do
      assert not_bound?(ascii_char([?a..?z]) |> times(min: 1) |> replace("OTHER"))
    end
  end

  describe "label/3 combinator at compile time" do
    defparsecp :compile_label, label(string("TO"), "label")
    defparsecp :compile_label_with_newline, label(string("T\nO"), "label")

    test "returns ok/error" do
      assert compile_label("TO") == {:ok, ["TO"], "", %{}, {1, 0}, 2}
      assert compile_label("TOC") == {:ok, ["TO"], "C", %{}, {1, 0}, 2}
      assert compile_label("AO") == {:error, "expected label", "AO", %{}, {1, 0}, 0}
    end

    test "properly counts newlines" do
      assert compile_label_with_newline("T\nO") == {:ok, ["T\nO"], "", %{}, {2, 2}, 3}
      assert compile_label_with_newline("T\nOC") == {:ok, ["T\nO"], "C", %{}, {2, 2}, 3}

      assert compile_label_with_newline("A\nO") ==
               {:error, "expected label", "A\nO", %{}, {1, 0}, 0}
    end

    test "is bound" do
      assert bound?(label(string("T"), "label"))
    end
  end

  describe "label/3 combinator at runtime" do
    defparsecp :runtime_label,
               label(times(ascii_char([?a..?z]), min: 1), "first label")
               |> label(times(ascii_char([?A..?Z]), min: 1), "second label")
               |> times(ascii_char([?0..?9]), min: 1)
               |> label("third label")

    test "returns ok/error" do
      assert runtime_label("aA0") == {:ok, [?a, ?A, ?0], "", %{}, {1, 0}, 3}

      error = "expected first label while processing third label"
      assert runtime_label("+A0") == {:error, error, "+A0", %{}, {1, 0}, 0}

      error = "expected second label while processing third label"
      assert runtime_label("a+0") == {:error, error, "+0", %{}, {1, 0}, 1}

      error = "expected third label"
      assert runtime_label("aA+") == {:error, error, "+", %{}, {1, 0}, 2}
    end

    test "is not bound" do
      assert not_bound?(ascii_char([?a..?z]) |> repeat() |> label("label"))
    end
  end

  describe "remote post_traverse/3 combinator" do
    @three_ascii_letters times(ascii_char([?a..?z]), min: 3)

    defparsecp :remote_post_traverse,
               string("T")
               |> integer(2)
               |> post_traverse(@three_ascii_letters, {__MODULE__, :public_join_and_wrap, ["-"]})
               |> integer(2)

    defparsecp :remote_post_traverse_error_when_last_is_z,
               post_traverse(@three_ascii_letters, {__MODULE__, :error_when_last_is_z, []})

    defparsecp :remote_post_traverse_lookahead,
               @three_ascii_letters
               |> wrap()
               |> post_traverse(string("#"), {__MODULE__, :from_code_to_code, []})
               |> integer(2)

    test "returns ok/error" do
      assert remote_post_traverse("T12abc34") ==
               {:ok, ["T", 12, "99-98-97", 34], "", %{}, {1, 0}, 8}

      error =
        "expected string \"T\", followed by ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert remote_post_traverse("Tabc34") == {:error, error, "Tabc34", %{}, {1, 0}, 0}

      error =
        "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert remote_post_traverse("T12abcdf") == {:error, error, "", %{}, {1, 0}, 8}

      error =
        "expected ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z'"

      assert remote_post_traverse("T12ab34") == {:error, error, "ab34", %{}, {1, 0}, 3}
    end

    test "returns error from traversal" do
      assert remote_post_traverse_error_when_last_is_z("abcdef") ==
               {:ok, 'abcdef', "", %{}, {1, 0}, 6}

      assert remote_post_traverse_error_when_last_is_z("abcdez") ==
               {:error, "last is z", "", %{}, {1, 0}, 6}
    end

    test "post traverrse with rest lookahead" do
      assert remote_post_traverse_lookahead("abcdef#XcanbeanythingX12") ==
               {:ok, ['abcdef', {"X", "canbeanything"}, 12], "", %{}, {1, 0}, 9}

      assert remote_post_traverse_lookahead("abcdef#ZcanbeanythingZ12") ==
               {:ok, ['abcdef', {"Z", "canbeanything"}, 12], "", %{}, {1, 0}, 9}

      assert remote_post_traverse_lookahead("abcdef#Zcanbeanything") ==
               {:error, "missing closing Z", "Zcanbeanything", %{}, {1, 0}, 7}
    end

    test "is not bound" do
      combinator = post_traverse(@three_ascii_letters, {__MODULE__, :public_join_and_wrap, ["-"]})
      assert not_bound?(combinator)
    end
  end

  describe "local post_traverse/3 combinator" do
    @three_ascii_letters times(ascii_char([?a..?z]), min: 3)

    defparsecp :local_post_traverse,
               string("T")
               |> integer(2)
               |> post_traverse(@three_ascii_letters, {:private_join_and_wrap, ["-"]})
               |> integer(2)

    defparsecp :local_post_traverse_error_when_last_is_z,
               post_traverse(@three_ascii_letters, {__MODULE__, :error_when_last_is_z, []})

    test "returns ok/error" do
      assert local_post_traverse("T12abc34") ==
               {:ok, ["T", 12, "99-98-97", 34], "", %{}, {1, 0}, 8}

      error =
        "expected string \"T\", followed by ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert local_post_traverse("Tabc34") == {:error, error, "Tabc34", %{}, {1, 0}, 0}

      error =
        "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert local_post_traverse("T12abcdf") == {:error, error, "", %{}, {1, 0}, 8}

      error =
        "expected ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z'"

      assert local_post_traverse("T12ab34") == {:error, error, "ab34", %{}, {1, 0}, 3}
    end

    test "returns error from traversal" do
      assert local_post_traverse_error_when_last_is_z("abcdef") ==
               {:ok, 'abcdef', "", %{}, {1, 0}, 6}

      assert local_post_traverse_error_when_last_is_z("abcdez") ==
               {:error, "last is z", "", %{}, {1, 0}, 6}
    end

    test "is not bound" do
      assert not_bound?(post_traverse(@three_ascii_letters, {:private_join_and_wrap, ["-"]}))
    end
  end

  describe "remote pre_traverse/3 combinator" do
    @three_ascii_letters times(ascii_char([?a..?z]), min: 3)

    defparsecp :remote_pre_traverse,
               string("T")
               |> integer(2)
               |> pre_traverse(@three_ascii_letters, {__MODULE__, :public_join_and_wrap, ["-"]})
               |> integer(2)

    defparsecp :remote_pre_traverse_error_when_last_is_z,
               pre_traverse(@three_ascii_letters, {__MODULE__, :error_when_last_is_z, []})

    test "returns ok/error" do
      assert remote_pre_traverse("T12abc34") ==
               {:ok, ["T", 12, "99-98-97", 34], "", %{}, {1, 0}, 8}

      error =
        "expected string \"T\", followed by ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert remote_pre_traverse("Tabc34") == {:error, error, "Tabc34", %{}, {1, 0}, 0}

      error =
        "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert remote_pre_traverse("T12abcdf") == {:error, error, "", %{}, {1, 0}, 8}

      error =
        "expected ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z'"

      assert remote_pre_traverse("T12ab34") == {:error, error, "ab34", %{}, {1, 0}, 3}
    end

    test "returns error from traversal" do
      assert remote_pre_traverse_error_when_last_is_z("abcdef") ==
               {:ok, 'abcdef', "", %{}, {1, 0}, 6}

      assert remote_pre_traverse_error_when_last_is_z("abcdez") ==
               {:error, "last is z", "", %{}, {1, 0}, 6}
    end

    test "is not bound" do
      combinator = pre_traverse(@three_ascii_letters, {__MODULE__, :public_join_and_wrap, ["-"]})
      assert not_bound?(combinator)
    end
  end

  describe "local pre_traverse/3 combinator" do
    @three_ascii_letters times(ascii_char([?a..?z]), min: 3)

    defparsecp :local_pre_traverse,
               string("T")
               |> integer(2)
               |> pre_traverse(@three_ascii_letters, {:private_join_and_wrap, ["-"]})
               |> integer(2)

    defparsecp :local_pre_traverse_error_when_last_is_z,
               pre_traverse(@three_ascii_letters, {__MODULE__, :error_when_last_is_z, []})

    test "returns ok/error" do
      assert local_pre_traverse("T12abc34") ==
               {:ok, ["T", 12, "99-98-97", 34], "", %{}, {1, 0}, 8}

      error =
        "expected string \"T\", followed by ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert local_pre_traverse("Tabc34") == {:error, error, "Tabc34", %{}, {1, 0}, 0}

      error =
        "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

      assert local_pre_traverse("T12abcdf") == {:error, error, "", %{}, {1, 0}, 8}

      error =
        "expected ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z'"

      assert local_pre_traverse("T12ab34") == {:error, error, "ab34", %{}, {1, 0}, 3}
    end

    test "returns error from traversal" do
      assert local_pre_traverse_error_when_last_is_z("abcdef") ==
               {:ok, 'abcdef', "", %{}, {1, 0}, 6}

      assert local_pre_traverse_error_when_last_is_z("abcdez") ==
               {:error, "last is z", "", %{}, {1, 0}, 6}
    end

    test "is not bound" do
      assert not_bound?(pre_traverse(@three_ascii_letters, {:private_join_and_wrap, ["-"]}))
    end
  end

  describe "pre_traverse/3 and post_traverse/3 locations" do
    defparsecp :bound_pre_and_post_traverse,
               string("ok")
               |> pre_traverse({:location, [:pre_traverse]})
               |> post_traverse({:location, [:post_traverse]})

    defparsecp :unbound_pre_and_post_traverse,
               integer(min: 2)
               |> pre_traverse({:location, [:pre_traverse]})
               |> post_traverse({:location, [:post_traverse]})

    test "when bound" do
      assert bound_pre_and_post_traverse("ok") ==
               {:ok, [{:post_traverse, [{:pre_traverse, ["ok"], {1, 0}, 0}], {1, 0}, 2}], "", %{},
                {1, 0}, 2}
    end

    test "when unbound" do
      assert unbound_pre_and_post_traverse("12") ==
               {:ok, [{:post_traverse, [{:pre_traverse, [12], {1, 0}, 0}], {1, 0}, 2}], "", %{},
                {1, 0}, 2}
    end
  end

  describe "lookahead/2" do
    defparsecp :lookahead_with_choice_digits_first,
               choice([
                 ascii_char([]) |> lookahead(integer(min: 1)) |> tag(:first),
                 ascii_char([]) |> lookahead(ascii_char([?a..?z])) |> tag(:second)
               ])

    defparsecp :lookahead_with_choice_digits_last,
               choice([
                 ascii_char([]) |> tag(:first) |> lookahead(ascii_char([?a..?z])),
                 ascii_char([]) |> tag(:second) |> lookahead(integer(min: 1))
               ])

    defparsecp :lookahead_with_inner_choice,
               ascii_char([]) |> lookahead(choice([ascii_char([?a..?c]), ascii_char([?d..?f])]))

    defparsecp :lookahead_with_times,
               times(ascii_char([]) |> lookahead(ascii_char([?0..?9])), min: 1)

    defparsecp :lookahead_with_inner_compound_combinator,
               lookahead(utf8_char([?a]) |> utf8_char([?b]) |> utf8_char([?c]))

    defparsecp :nested_lookahead_with_inner_compound_combinator,
               lookahead(
                 utf8_char([?a])
                 |> lookahead(utf8_char([?b]) |> utf8_char([?c]))
               )

    test "matches inner combinators in order" do
      assert lookahead_with_inner_compound_combinator("abc") ==
               {:ok, [], "abc", %{}, {1, 0}, 0}

      assert nested_lookahead_with_inner_compound_combinator("abc") ==
               {:ok, [], "abc", %{}, {1, 0}, 0}
    end

    test "aborts choice on no match" do
      assert lookahead_with_choice_digits_first("a0") == {:ok, [first: 'a'], "0", %{}, {1, 0}, 1}
      assert lookahead_with_choice_digits_first("aa") == {:ok, [second: 'a'], "a", %{}, {1, 0}, 1}
      assert lookahead_with_choice_digits_last("a0") == {:ok, [second: 'a'], "0", %{}, {1, 0}, 1}
      assert lookahead_with_choice_digits_last("aa") == {:ok, [first: 'a'], "a", %{}, {1, 0}, 1}
    end

    test "with inner choice" do
      assert lookahead_with_inner_choice("aa") == {:ok, 'a', "a", %{}, {1, 0}, 1}
      assert lookahead_with_inner_choice("af") == {:ok, 'a', "f", %{}, {1, 0}, 1}

      assert lookahead_with_inner_choice("az") ==
               {:error,
                "expected ASCII character in the range 'a' to 'c' or ASCII character in the range 'd' to 'f'",
                "z", %{}, {1, 0}, 1}
    end

    test "aborts times" do
      assert lookahead_with_times("a") ==
               {:error, "expected ASCII character in the range '0' to '9'", "", %{}, {1, 0}, 1}

      assert lookahead_with_times("a0") == {:ok, 'a', "0", %{}, {1, 0}, 1}

      assert lookahead_with_times("aa0") ==
               {:error, "expected ASCII character in the range '0' to '9'", "a0", %{}, {1, 0}, 1}
    end
  end

  describe "lookahead_not/2" do
    defparsecp :lookahead_not_with_choice_digits_first,
               choice([
                 ascii_char([]) |> lookahead_not(integer(min: 1)) |> tag(:first),
                 ascii_char([]) |> lookahead_not(ascii_char([?a..?z])) |> tag(:second)
               ])

    defparsecp :lookahead_not_with_choice_digits_last,
               choice([
                 ascii_char([]) |> tag(:first) |> lookahead_not(ascii_char([?a..?z])),
                 ascii_char([]) |> tag(:second) |> lookahead_not(integer(min: 1))
               ])

    defparsecp :lookahead_not_with_inner_choice,
               ascii_char([])
               |> lookahead_not(choice([ascii_char([?a..?c]), ascii_char([?d..?f])]))

    defparsecp :lookahead_not_with_times,
               times(ascii_char([]) |> lookahead_not(ascii_char([?0..?9])), min: 1)

    defparsecp :lookahead_not_repeat_until,
               repeat(lookahead_not(string("3")) |> ascii_char([?0..?9]) |> ascii_char([?0..?9]))

    defparsecp :lookahead_not_with_inner_compound_combinator,
               lookahead_not(utf8_char([?a]) |> utf8_char([?b]))

    defparsecp :nested_lookahead_not_with_inner_compound_combinator,
               lookahead_not(
                 utf8_char([?a])
                 |> lookahead(utf8_char([?b]) |> utf8_char([?c]))
               )

    test "matches inner combinators in order" do
      assert lookahead_not_with_inner_compound_combinator("ab") ==
               {:error,
                "did not expect utf8 codepoint equal to 'a', followed by utf8 codepoint equal to 'b'",
                "ab", %{}, {1, 0}, 0}

      assert nested_lookahead_not_with_inner_compound_combinator("abc") ==
               {:error,
                "did not expect utf8 codepoint equal to 'a', followed by utf8 codepoint equal to 'b', followed by utf8 codepoint equal to 'c'",
                "abc", %{}, {1, 0}, 0}
    end

    test "aborts choice on match" do
      assert lookahead_not_with_choice_digits_first("a0") ==
               {:ok, [second: 'a'], "0", %{}, {1, 0}, 1}

      assert lookahead_not_with_choice_digits_first("aa") ==
               {:ok, [first: 'a'], "a", %{}, {1, 0}, 1}

      assert lookahead_not_with_choice_digits_last("a0") ==
               {:ok, [first: 'a'], "0", %{}, {1, 0}, 1}

      assert lookahead_not_with_choice_digits_last("aa") ==
               {:ok, [second: 'a'], "a", %{}, {1, 0}, 1}
    end

    test "with inner choice" do
      assert lookahead_not_with_inner_choice("az") == {:ok, 'a', "z", %{}, {1, 0}, 1}

      assert lookahead_not_with_inner_choice("aa") ==
               {:error,
                "did not expect ASCII character in the range 'a' to 'c' or ASCII character in the range 'd' to 'f'",
                "a", %{}, {1, 0}, 1}

      assert lookahead_not_with_inner_choice("af") ==
               {:error,
                "did not expect ASCII character in the range 'a' to 'c' or ASCII character in the range 'd' to 'f'",
                "f", %{}, {1, 0}, 1}
    end

    test "aborts times" do
      assert lookahead_not_with_times("a0") ==
               {:error, "did not expect ASCII character in the range '0' to '9'", "0", %{},
                {1, 0}, 1}

      assert lookahead_not_with_times("aa0") == {:ok, 'a', "a0", %{}, {1, 0}, 1}
      assert lookahead_not_with_times("aaa0") == {:ok, 'aa', "a0", %{}, {1, 0}, 2}
    end

    test "repeaet_until" do
      assert lookahead_not_repeat_until("1245") == {:ok, [?1, ?2, ?4, ?5], "", %{}, {1, 0}, 4}
      assert lookahead_not_repeat_until("12345") == {:ok, [?1, ?2], "345", %{}, {1, 0}, 2}
      assert lookahead_not_repeat_until("135") == {:ok, [?1, ?3], "5", %{}, {1, 0}, 2}
      assert lookahead_not_repeat_until("312") == {:ok, [], "312", %{}, {1, 0}, 0}
      assert lookahead_not_repeat_until("a123") == {:ok, [], "a123", %{}, {1, 0}, 0}
    end
  end

  describe "line/2 combinator" do
    defparsecp :ascii_line,
               ascii_char([])
               |> ascii_char([])
               |> ascii_char([])
               |> line()

    test "returns ok/error" do
      assert ascii_line("abc") == {:ok, [{[?a, ?b, ?c], {1, 0}}], "", %{}, {1, 0}, 3}
      assert ascii_line("a\nc") == {:ok, [{[?a, ?\n, ?c], {2, 2}}], "", %{}, {2, 2}, 3}
    end
  end

  describe "byte_offset/2 combinator" do
    defparsecp :ascii_byte_offset,
               ascii_char([])
               |> ascii_char([])
               |> ascii_char([])
               |> byte_offset()

    test "returns ok/error" do
      assert ascii_byte_offset("abc") == {:ok, [{[?a, ?b, ?c], 3}], "", %{}, {1, 0}, 3}
      assert ascii_byte_offset("a\nc") == {:ok, [{[?a, ?\n, ?c], 3}], "", %{}, {2, 2}, 3}
    end
  end

  describe "wrap/2 combinator" do
    defparsecp :two_integers_wrapped,
               integer(1)
               |> integer(1)
               |> wrap()

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

    test "returns ok/error" do
      assert two_integers_wrapped("12") == {:ok, [[1, 2]], "", %{}, {1, 0}, 2}
      assert two_integers_wrapped("123") == {:ok, [[1, 2]], "3", %{}, {1, 0}, 2}
      assert two_integers_wrapped("a12") == {:error, @error, "a12", %{}, {1, 0}, 0}
    end
  end

  describe "tag/3 combinator" do
    defparsecp :two_integers_tagged,
               integer(1)
               |> integer(1)
               |> tag(:ints)

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

    test "returns ok/error" do
      assert two_integers_tagged("12") == {:ok, [{:ints, [1, 2]}], "", %{}, {1, 0}, 2}
      assert two_integers_tagged("123") == {:ok, [{:ints, [1, 2]}], "3", %{}, {1, 0}, 2}
      assert two_integers_tagged("a12") == {:error, @error, "a12", %{}, {1, 0}, 0}
    end
  end

  describe "unwrap_and_tag/3 combinator" do
    defparsecp :maybe_two_integers_unwrapped_and_tagged,
               integer(1)
               |> optional(integer(1))
               |> unwrap_and_tag(:ints)

    @error "expected ASCII character in the range '0' to '9'"

    test "returns ok/error" do
      assert maybe_two_integers_unwrapped_and_tagged("1") ==
               {:ok, [{:ints, 1}], "", %{}, {1, 0}, 1}

      assert maybe_two_integers_unwrapped_and_tagged("a") == {:error, @error, "a", %{}, {1, 0}, 0}

      assert_raise RuntimeError,
                   ~r"unwrap_and_tag/3 expected a single token, got: \[1, 2\]",
                   fn -> maybe_two_integers_unwrapped_and_tagged("12") end
    end
  end

  describe "debug/2 combinator" do
    defparsecp :two_integers_debugged,
               integer(1)
               |> integer(1)
               |> debug()

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9'"

    test "returns ok/error" do
      debug =
        capture_io(fn ->
          assert two_integers_debugged("123") == {:ok, [1, 2], "3", %{}, {1, 0}, 2}
        end)

      assert debug == """
             == DEBUG ==
             Bin: "3"
             Acc: [1, 2]
             Ctx: %{}
             Lin: {1, 0}
             Off: 0

             """
    end
  end

  describe "remote map/3 combinator" do
    defparsecp :remote_map,
               ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> map({Integer, :to_string, []})

    defparsecp :empty_map, map(empty(), {Integer, :to_string, []})

    test "returns ok/error" do
      assert remote_map("abc") == {:ok, ["97", "98", "99"], "", %{}, {1, 0}, 3}
      assert remote_map("abcd") == {:ok, ["97", "98", "99"], "d", %{}, {1, 0}, 3}
      assert {:error, _, "1abcd", %{}, {1, 0}, 0} = remote_map("1abcd")
    end

    test "can map empty" do
      assert empty_map("abc") == {:ok, [], "abc", %{}, {1, 0}, 0}
    end
  end

  describe "local map/3 combinator" do
    defparsecp :local_map,
               ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> map({:local_to_string, []})

    test "returns ok/error" do
      assert local_map("abc") == {:ok, ["97", "98", "99"], "", %{}, {1, 0}, 3}
      assert local_map("abcd") == {:ok, ["97", "98", "99"], "d", %{}, {1, 0}, 3}
      assert {:error, _, "1abcd", %{}, {1, 0}, 0} = local_map("1abcd")
    end

    defp local_to_string(arg) do
      Integer.to_string(arg)
    end
  end

  describe "remote reduce/3 combinator" do
    defparsecp :remote_reduce,
               ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> reduce({Enum, :join, ["-"]})

    defparsecp :empty_reduce, reduce(empty(), {Enum, :join, ["-"]})

    test "returns ok/error" do
      assert remote_reduce("abc") == {:ok, ["97-98-99"], "", %{}, {1, 0}, 3}
      assert remote_reduce("abcd") == {:ok, ["97-98-99"], "d", %{}, {1, 0}, 3}
      assert {:error, _, "1abcd", %{}, {1, 0}, 0} = remote_reduce("1abcd")
    end

    test "can reduce empty" do
      assert empty_reduce("abc") == {:ok, [""], "abc", %{}, {1, 0}, 0}
    end
  end

  describe "local reduce/3 combinator" do
    defparsecp :local_reduce,
               ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> ascii_char([?a..?z])
               |> reduce({:local_join, ["-"]})

    test "returns ok/error" do
      assert local_reduce("abc") == {:ok, ["97-98-99"], "", %{}, {1, 0}, 3}
      assert local_reduce("abcd") == {:ok, ["97-98-99"], "d", %{}, {1, 0}, 3}
      assert {:error, _, "1abcd", %{}, {1, 0}, 0} = local_reduce("1abcd")
    end

    defp local_join(list, joiner) do
      Enum.join(list, joiner)
    end
  end

  describe "concat/2 combinator" do
    defparsecp :concat_digit_upper_lower_plus,
               concat(
                 concat(ascii_char([?0..?9]), ascii_char([?A..?Z])),
                 concat(ascii_char([?a..?z]), ascii_char([?+..?+]))
               )

    test "returns ok/error" do
      assert concat_digit_upper_lower_plus("1Az+") == {:ok, [?1, ?A, ?z, ?+], "", %{}, {1, 0}, 4}
    end
  end

  describe "repeat/2 combinator" do
    defparsecp :repeat_digits, repeat(ascii_char([?0..?9]) |> ascii_char([?0..?9]))

    ascii_to_string = map(ascii_char([?0..?9]), :to_string)
    defparsecp :repeat_digits_to_string, repeat(ascii_to_string)

    defparsecp :repeat_digits_to_same_inner,
               repeat(map(ascii_to_string, {String, :to_integer, []}))

    defparsecp :repeat_digits_to_same_outer,
               map(repeat(ascii_to_string), {String, :to_integer, []})

    defparsecp :repeat_double_digits_to_string,
               repeat(
                 concat(
                   map(ascii_char([?0..?9]), :to_string),
                   map(ascii_char([?0..?9]), :to_string)
                 )
               )

    test "returns ok/error" do
      assert repeat_digits("12") == {:ok, [?1, ?2], "", %{}, {1, 0}, 2}
      assert repeat_digits("123") == {:ok, [?1, ?2], "3", %{}, {1, 0}, 2}
      assert repeat_digits("a123") == {:ok, [], "a123", %{}, {1, 0}, 0}
    end

    test "returns ok/error with map" do
      assert repeat_digits_to_string("123") == {:ok, ["49", "50", "51"], "", %{}, {1, 0}, 3}
    end

    test "returns ok/error with inner and outer map" do
      assert repeat_digits_to_same_inner("123") == {:ok, [?1, ?2, ?3], "", %{}, {1, 0}, 3}
      assert repeat_digits_to_same_outer("123") == {:ok, [?1, ?2, ?3], "", %{}, {1, 0}, 3}
    end

    test "returns ok/error with concat map" do
      assert repeat_double_digits_to_string("12") == {:ok, ["49", "50"], "", %{}, {1, 0}, 2}
      assert repeat_double_digits_to_string("123") == {:ok, ["49", "50"], "3", %{}, {1, 0}, 2}
      assert repeat_double_digits_to_string("a123") == {:ok, [], "a123", %{}, {1, 0}, 0}
    end
  end

  describe "repeat_while/3 combinator" do
    defparsecp :repeat_while_digits,
               repeat_while(
                 ascii_char([?0..?9]) |> ascii_char([?0..?9]),
                 {__MODULE__, :not_3, []}
               )

    ascii_to_string = map(ascii_char([?0..?9]), :to_string)
    defparsecp :repeat_while_digits_to_string, repeat_while(ascii_to_string, {:not_3, []})

    defparsecp :repeat_while_digits_to_same_inner,
               repeat_while(map(ascii_to_string, {String, :to_integer, []}), {:not_3, []})

    defparsecp :repeat_while_digits_to_same_outer,
               map(repeat_while(ascii_to_string, {:not_3, []}), {String, :to_integer, []})

    defparsecp :repeat_while_double_digits_to_string,
               repeat_while(
                 concat(
                   map(ascii_char([?0..?9]), :to_string),
                   map(ascii_char([?0..?9]), :to_string)
                 ),
                 {:not_3, []}
               )

    defparsecp :repeat_while_repeat_digit,
               repeat_while(
                 concat(ascii_char([?a..?z]), repeat(ascii_char([?0..?9]))),
                 {:n_times, []}
               )

    test "returns ok/error" do
      assert repeat_while_digits("1245") == {:ok, [?1, ?2, ?4, ?5], "", %{}, {1, 0}, 4}
      assert repeat_while_digits("12345") == {:ok, [?1, ?2], "345", %{}, {1, 0}, 2}
      assert repeat_while_digits("135") == {:ok, [?1, ?3], "5", %{}, {1, 0}, 2}
      assert repeat_while_digits("312") == {:ok, [], "312", %{}, {1, 0}, 0}
      assert repeat_while_digits("a123") == {:ok, [], "a123", %{}, {1, 0}, 0}
    end

    test "returns ok/error with map" do
      assert repeat_while_digits_to_string("123") == {:ok, ["49", "50"], "3", %{}, {1, 0}, 2}
      assert repeat_while_digits_to_string("321") == {:ok, [], "321", %{}, {1, 0}, 0}
    end

    test "returns ok/error with inner and outer map" do
      assert repeat_while_digits_to_same_inner("123") == {:ok, [?1, ?2], "3", %{}, {1, 0}, 2}
      assert repeat_while_digits_to_same_outer("123") == {:ok, [?1, ?2], "3", %{}, {1, 0}, 2}

      assert repeat_while_digits_to_same_inner("321") == {:ok, [], "321", %{}, {1, 0}, 0}
      assert repeat_while_digits_to_same_outer("321") == {:ok, [], "321", %{}, {1, 0}, 0}
    end

    test "returns ok/error with concat map" do
      assert repeat_while_double_digits_to_string("12345") ==
               {:ok, ["49", "50"], "345", %{}, {1, 0}, 2}

      assert repeat_while_double_digits_to_string("135") ==
               {:ok, ["49", "51"], "5", %{}, {1, 0}, 2}

      assert repeat_while_double_digits_to_string("312") == {:ok, [], "312", %{}, {1, 0}, 0}
      assert repeat_while_double_digits_to_string("a123") == {:ok, [], "a123", %{}, {1, 0}, 0}
    end

    test "returns ok/error with repeat" do
      assert repeat_while_repeat_digit("a1b2c3d4e5f6g7h8", context: %{count: 3}) ==
               {:ok, 'a1b2', "c3d4e5f6g7h8", %{count: 0}, {1, 0}, 4}
    end

    def not_3(<<?3, _::binary>>, %{} = context, {line, line_offset}, byte_offset)
        when is_integer(line) and is_integer(line_offset) and is_integer(byte_offset) do
      {:halt, context}
    end

    def not_3(<<_::binary>>, %{} = context, {line, line_offset}, byte_offset)
        when is_integer(line) and is_integer(line_offset) and is_integer(byte_offset) do
      {:cont, context}
    end

    def n_times(_, context, _, _) do
      if context.count > 0 do
        {:cont, %{context | count: context.count - 1}}
      else
        {:halt, context}
      end
    end
  end

  describe "times/2 combinator" do
    defparsecp :times_digits, times(ascii_char([?0..?9]) |> ascii_char([?0..?9]), max: 4)
    defparsecp :times_choice, times(choice([ascii_char([?0..?4]), ascii_char([?5..?9])]), max: 4)

    defparsecp :choice_times,
               choice([
                 times(ascii_char([?0..?9]), min: 1, max: 4),
                 times(ascii_char([?a..?z]), min: 1, max: 4)
               ])

    test "returns ok/error when bound" do
      assert times_digits("12") == {:ok, [?1, ?2], "", %{}, {1, 0}, 2}
      assert times_digits("123") == {:ok, [?1, ?2], "3", %{}, {1, 0}, 2}

      assert times_digits("123456789") ==
               {:ok, [?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8], "9", %{}, {1, 0}, 8}

      assert times_digits("1234567890") ==
               {:ok, [?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8], "90", %{}, {1, 0}, 8}

      assert times_digits("12o") == {:ok, [?1, ?2], "o", %{}, {1, 0}, 2}
      assert times_digits("o") == {:ok, [], "o", %{}, {1, 0}, 0}
    end

    test "returns ok/error with choice" do
      assert times_choice("12") == {:ok, [?1, ?2], "", %{}, {1, 0}, 2}
      assert times_choice("123") == {:ok, [?1, ?2, ?3], "", %{}, {1, 0}, 3}
      assert times_choice("12345") == {:ok, [?1, ?2, ?3, ?4], "5", %{}, {1, 0}, 4}
      assert times_choice("12o") == {:ok, [?1, ?2], "o", %{}, {1, 0}, 2}
      assert times_choice("o") == {:ok, [], "o", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character in the range '0' to '9', followed by ASCII character in the range '0' to '9' or ASCII character in the range 'a' to 'z', followed by ASCII character in the range 'a' to 'z'"

    test "returns ok/error with outer choice" do
      assert choice_times("12") == {:ok, [?1, ?2], "", %{}, {1, 0}, 2}
      assert choice_times("12a") == {:ok, [?1, ?2], "a", %{}, {1, 0}, 2}
      assert choice_times("12345") == {:ok, [?1, ?2, ?3, ?4], "5", %{}, {1, 0}, 4}
      assert choice_times("ab") == {:ok, [?a, ?b], "", %{}, {1, 0}, 2}
      assert choice_times("ab1") == {:ok, [?a, ?b], "1", %{}, {1, 0}, 2}
      assert choice_times("abcde") == {:ok, [?a, ?b, ?c, ?d], "e", %{}, {1, 0}, 4}
      assert choice_times("+") == {:error, @error, "+", %{}, {1, 0}, 0}
    end
  end

  describe "duplicate/3 combinator" do
    defparsecp :duplicate_twice, ascii_char([?0..?9]) |> duplicate(ascii_char([?a..?z]), 2)
    defparsecp :duplicate_zero, ascii_char([?0..?9]) |> duplicate(ascii_char([?a..?z]), 0)

    test "returns ok/error when bound" do
      assert duplicate_twice("0ab") == {:ok, [?0, ?a, ?b], "", %{}, {1, 0}, 3}
      assert duplicate_zero("0ab") == {:ok, [?0], "ab", %{}, {1, 0}, 1}
    end
  end

  describe "choice/2 combinator" do
    defparsecp :simple_choice,
               choice([ascii_char([?a..?z]), ascii_char([?A..?Z]), ascii_char([?0..?9])])

    defparsecp :choice_label,
               choice([ascii_char([?a..?z]), ascii_char([?A..?Z]), ascii_char([?0..?9])])
               |> label("something")

    defparsecp :choice_inner_repeat,
               choice([repeat(ascii_char([?a..?z])), repeat(ascii_char([?A..?Z]))])

    defparsecp :choice_outer_repeat, repeat(choice([ascii_char([?a..?z]), ascii_char([?A..?Z])]))

    defparsecp :choice_repeat_and_inner_map,
               repeat(
                 choice([
                   map(ascii_char([?a..?z]), :to_string),
                   map(ascii_char([?A..?Z]), :to_string)
                 ])
               )

    defparsecp :choice_repeat_and_maps,
               map(
                 repeat(
                   choice([
                     map(ascii_char([?a..?z]), :to_string),
                     map(ascii_char([?A..?Z]), :to_string)
                   ])
                 ),
                 {String, :to_integer, []}
               )

    defparsecp :choice_with_empty,
               choice([
                 ascii_char([?a..?z]),
                 empty()
               ])

    @error "expected ASCII character in the range 'a' to 'z' or ASCII character in the range 'A' to 'Z' or ASCII character in the range '0' to '9'"

    test "returns ok/error" do
      assert simple_choice("a=") == {:ok, [?a], "=", %{}, {1, 0}, 1}
      assert simple_choice("A=") == {:ok, [?A], "=", %{}, {1, 0}, 1}
      assert simple_choice("0=") == {:ok, [?0], "=", %{}, {1, 0}, 1}
      assert simple_choice("+=") == {:error, @error, "+=", %{}, {1, 0}, 0}
    end

    @error "expected something"

    test "returns ok/error with wrapping label" do
      assert choice_label("a=") == {:ok, [?a], "=", %{}, {1, 0}, 1}
      assert choice_label("A=") == {:ok, [?A], "=", %{}, {1, 0}, 1}
      assert choice_label("0=") == {:ok, [?0], "=", %{}, {1, 0}, 1}
      assert choice_label("+=") == {:error, @error, "+=", %{}, {1, 0}, 0}
    end

    test "returns ok/error with repeat inside" do
      assert choice_inner_repeat("az") == {:ok, [?a, ?z], "", %{}, {1, 0}, 2}
      assert choice_inner_repeat("AZ") == {:ok, [], "AZ", %{}, {1, 0}, 0}
    end

    test "returns ok/error with repeat outside" do
      assert choice_outer_repeat("az") == {:ok, [?a, ?z], "", %{}, {1, 0}, 2}
      assert choice_outer_repeat("AZ") == {:ok, [?A, ?Z], "", %{}, {1, 0}, 2}
      assert choice_outer_repeat("aAzZ") == {:ok, [?a, ?A, ?z, ?Z], "", %{}, {1, 0}, 4}
    end

    test "returns ok/error with repeat and inner map" do
      assert choice_repeat_and_inner_map("az") == {:ok, ["97", "122"], "", %{}, {1, 0}, 2}
      assert choice_repeat_and_inner_map("AZ") == {:ok, ["65", "90"], "", %{}, {1, 0}, 2}

      assert choice_repeat_and_inner_map("aAzZ") ==
               {:ok, ["97", "65", "122", "90"], "", %{}, {1, 0}, 4}
    end

    test "returns ok/error with repeat and maps" do
      assert choice_repeat_and_maps("az") == {:ok, [?a, ?z], "", %{}, {1, 0}, 2}
      assert choice_repeat_and_maps("AZ") == {:ok, [?A, ?Z], "", %{}, {1, 0}, 2}
      assert choice_repeat_and_maps("aAzZ") == {:ok, [?a, ?A, ?z, ?Z], "", %{}, {1, 0}, 4}
    end

    test "returns ok/error on empty" do
      assert choice_with_empty("az") == {:ok, [?a], "z", %{}, {1, 0}, 1}
      assert choice_with_empty("AZ") == {:ok, [], "AZ", %{}, {1, 0}, 0}
    end
  end

  describe "optional/2 combinator" do
    defparsecp :optional_ascii, optional(ascii_char([?a..?z]))

    test "returns ok/error on empty" do
      assert optional_ascii("az") == {:ok, [?a], "z", %{}, {1, 0}, 1}
      assert optional_ascii("AZ") == {:ok, [], "AZ", %{}, {1, 0}, 0}
    end
  end

  describe "eventually/2 combinator" do
    hour = integer(min: 1, max: 2) |> label("hour")
    defparsecp :eventually_integer, eventually(hour)
    defparsecp :repeat_eventually, repeat(eventually(hour))

    defparsecp :eventually_complex,
               ascii_string([?a..?z], min: 1)
               |> integer(min: 1)
               |> eventually()

    test "returns ok/error" do
      assert eventually_integer("let's meet at 12?") == {:ok, [12], "?", %{}, {1, 0}, 16}

      assert eventually_integer("let's not meet") ==
               {:error, "expected hour", "", %{}, {1, 0}, 14}
    end

    test "returns ok/error with repeat" do
      assert repeat_eventually("let's meet at 12? or at 14!") ==
               {:ok, [12, 14], "!", %{}, {1, 0}, 26}

      assert repeat_eventually("let's not meet") == {:ok, [], "let's not meet", %{}, {1, 0}, 0}
    end

    test "returns ok/error with complex" do
      assert eventually_complex("ohno\nmatch13\n") == {:ok, ["match", 13], "\n", %{}, {2, 5}, 12}
    end
  end

  describe "parsec/2 combinator" do
    defcombinatorp :parsec_inner,
                   choice([
                     map(ascii_char([?a..?z]), {:to_string, []}),
                     map(ascii_char([?A..?Z]), {:to_string, []})
                   ])

    defparsecp :parsec_string, string("T") |> parsec(:parsec_inner) |> string("O")
    defparsecp :parsec_repeat, repeat(parsec(:parsec_inner))
    defparsecp :parsec_map, map(parsec(:parsec_inner), {String, :to_integer, []})
    defparsecp :parsec_choice, choice([parsec(:parsec_inner), string("+")])
    defparsecp :parsec_repeat_map, repeat(map(parsec(:parsec_inner), {String, :to_integer, []}))

    test "returns ok/error with string" do
      assert parsec_string("TaO") == {:ok, ["T", "97", "O"], "", %{}, {1, 0}, 3}

      error = "expected string \"T\""
      assert parsec_string("ZaO") == {:error, error, "ZaO", %{}, {1, 0}, 0}

      error =
        "expected ASCII character in the range 'a' to 'z' or ASCII character in the range 'A' to 'Z'"

      assert parsec_string("T1O") == {:error, error, "1O", %{}, {1, 0}, 1}

      error = "expected string \"O\""
      assert parsec_string("TaA") == {:error, error, "A", %{}, {1, 0}, 2}
    end

    test "returns ok/error with choice" do
      assert parsec_choice("+O") == {:ok, ["+"], "O", %{}, {1, 0}, 1}
      assert parsec_choice("O+") == {:ok, ["79"], "+", %{}, {1, 0}, 1}

      assert parsec_choice("==") ==
               {:error, "expected parsec_inner or string \"+\"", "==", %{}, {1, 0}, 0}
    end

    test "returns ok/error with repeat" do
      assert parsec_repeat("az") == {:ok, ["97", "122"], "", %{}, {1, 0}, 2}
      assert parsec_repeat("AZ") == {:ok, ["65", "90"], "", %{}, {1, 0}, 2}
      assert parsec_repeat("aAzZ") == {:ok, ["97", "65", "122", "90"], "", %{}, {1, 0}, 4}
      assert parsec_repeat("1aAzZ") == {:ok, [], "1aAzZ", %{}, {1, 0}, 0}
    end

    @error "expected ASCII character in the range 'a' to 'z' or ASCII character in the range 'A' to 'Z'"

    test "returns ok/error with map" do
      assert parsec_map("az") == {:ok, [?a], "z", %{}, {1, 0}, 1}
      assert parsec_map("AZ") == {:ok, [?A], "Z", %{}, {1, 0}, 1}
      assert parsec_map("1aAzZ") == {:error, @error, "1aAzZ", %{}, {1, 0}, 0}
    end

    test "returns ok/error with repeat + map" do
      assert parsec_repeat_map("az") == {:ok, [?a, ?z], "", %{}, {1, 0}, 2}
      assert parsec_repeat_map("AZ") == {:ok, [?A, ?Z], "", %{}, {1, 0}, 2}
      assert parsec_repeat_map("1aAzZ") == {:ok, [], "1aAzZ", %{}, {1, 0}, 0}
    end
  end

  describe "parsec/1 with remote combinator" do
    defmodule Remote do
      defparsec :parsec_export,
                choice([
                  map(ascii_char([?a..?z]), {:to_string, []}),
                  map(ascii_char([?A..?Z]), {:to_string, []})
                ]),
                export_combinator: true

      defcombinator :parsec_remote,
                    choice([
                      map(ascii_char([?a..?z]), {:to_string, []}),
                      map(ascii_char([?A..?Z]), {:to_string, []})
                    ])
    end

    defparsecp :parsec_remote_repeat, repeat(parsec({Remote, :parsec_remote}))
    defparsecp :parsec_export_repeat, repeat(parsec({Remote, :parsec_export}))

    test "returns ok/error with repeat" do
      assert parsec_remote_repeat("az") == {:ok, ["97", "122"], "", %{}, {1, 0}, 2}
      assert parsec_remote_repeat("AZ") == {:ok, ["65", "90"], "", %{}, {1, 0}, 2}
      assert parsec_remote_repeat("aAzZ") == {:ok, ["97", "65", "122", "90"], "", %{}, {1, 0}, 4}
      assert parsec_remote_repeat("1aAzZ") == {:ok, [], "1aAzZ", %{}, {1, 0}, 0}

      assert parsec_export_repeat("az") == {:ok, ["97", "122"], "", %{}, {1, 0}, 2}
      assert parsec_export_repeat("AZ") == {:ok, ["65", "90"], "", %{}, {1, 0}, 2}
      assert parsec_export_repeat("aAzZ") == {:ok, ["97", "65", "122", "90"], "", %{}, {1, 0}, 4}
      assert parsec_export_repeat("1aAzZ") == {:ok, [], "1aAzZ", %{}, {1, 0}, 0}
    end
  end

  describe "eos/1 combinator" do
    defparsecp :only_eos, eos()
    defparsecp :multi_eos, eos() |> eos()
    defparsecp :bad_eos, ascii_char([?a..?z]) |> eos() |> ascii_char([?a..?z])

    @error "expected end of string"

    test "returns ok/error" do
      assert only_eos("") == {:ok, [], "", %{}, {1, 0}, 0}
      assert only_eos("a") == {:error, @error, "a", %{}, {1, 0}, 0}
      assert multi_eos("") == {:ok, [], "", %{}, {1, 0}, 0}
    end

    test "never succeeds on bad eos" do
      assert bad_eos("a") ==
               {:error, "expected ASCII character in the range 'a' to 'z'", "", %{}, {1, 0}, 1}

      assert bad_eos("aa") ==
               {:error,
                "expected ASCII character in the range 'a' to 'z', followed by end of string",
                "aa", %{}, {1, 0}, 0}
    end
  end

  describe "continuing parser" do
    defparsecp :digits, [?0..?9] |> ascii_char() |> times(min: 1) |> label("digits")
    defparsecp :chars, [?a..?z] |> ascii_char() |> times(min: 1) |> label("chars")

    test "returns ok" do
      string = "123abc"
      assert {:ok, '123', "abc" = rest, %{}, {1, 0} = line, byte_offset} = digits(string)
      assert chars(rest, line: line, byte_offset: byte_offset) == {:ok, 'abc', "", %{}, {1, 0}, 6}
    end

    test "returns error" do
      string = "123:abc"
      assert {:ok, '123', ":abc" = rest, %{}, {1, 0} = line, byte_offset} = digits(string)

      assert chars(rest, line: line, byte_offset: byte_offset) ==
               {:error, "expected chars", ":abc", %{}, {1, 0}, 3}
    end
  end

  defp location(_rest, args, %{} = context, line, offset, tag) do
    {[{tag, args, line, offset}], context}
  end

  defp bound?(document) do
    {defs, _} = NimbleParsec.Compiler.compile(:not_used, document, [])

    assert length(defs) == 3,
           "Expected #{inspect(document)} to contain 3 clauses, got #{length(defs)}"
  end

  defp not_bound?(document) do
    {defs, _} = NimbleParsec.Compiler.compile(:not_used, document, [])

    assert length(defs) != 3, "Expected #{inspect(document)} to contain greater than 3 clauses"
  end

  def error_when_last_is_z(rest, acc, %{} = context, {line, line_offset}, byte_offset)
      when is_binary(rest) and is_integer(line) and is_integer(line_offset) and
             is_integer(byte_offset) do
    case acc do
      [?z | _] -> {:error, "last is z"}
      acc -> {rest, acc, context}
    end
  end

  def error_when_next_is_0(rest, %{} = context, {line, line_offset}, byte_offset)
      when is_integer(line) and is_integer(line_offset) and is_integer(byte_offset) do
    case rest do
      <<?0, _::binary>> -> {:error, "next is 0"}
      _ -> {rest, [], context}
    end
  end

  def from_code_to_code(rest, _tokens, %{} = context, {line, line_offset}, byte_offset)
      when is_binary(rest) and is_integer(line) and is_integer(line_offset) and
             is_integer(byte_offset) do
    <<code, rest::binary>> = rest

    case :binary.split(rest, <<code>>) do
      [prefix, rest] -> {rest, [{<<code>>, prefix}], context}
      [_] -> {:error, "missing closing #{<<code>>}"}
    end
  end

  def public_join_and_wrap(rest, args, %{} = context, {line, line_offset}, byte_offset, joiner)
      when is_binary(rest) and is_integer(line) and is_integer(line_offset) and
             is_integer(byte_offset) do
    {rest, args |> Enum.join(joiner) |> List.wrap(), context}
  end

  defp private_join_and_wrap(rest, args, %{} = context, {line, line_offset}, byte_offset, joiner)
       when is_binary(rest) and is_integer(line) and is_integer(line_offset) and
              is_integer(byte_offset) do
    {args |> Enum.join(joiner) |> List.wrap(), context}
  end
end
