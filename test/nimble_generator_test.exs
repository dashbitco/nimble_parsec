defmodule NimbleGeneratorTest do
  use ExUnit.Case, async: true

  import NimbleParsec

  describe "ascii_char" do
    test "unbound" do
      for _ <- 1..10 do
        assert <<_>> = ascii_char([]) |> generate()
      end
    end

    test "inclusive range" do
      for _ <- 1..10 do
        assert <<x>> = ascii_char([?0..?9]) |> generate()
        assert x in ?0..?9
      end
    end

    test "inclusive chars" do
      for _ <- 1..10 do
        assert <<x>> = ascii_char(Enum.to_list(?0..?9)) |> generate()
        assert x in ?0..?9
      end
    end

    test "exclusive range" do
      for _ <- 1..10 do
        assert "0" = ascii_char([?0..?9, not: ?1..?9]) |> generate()
      end
    end

    test "exclusive chars" do
      for _ <- 1..10 do
        assert "0" = ascii_char([?0..?9] ++ Enum.map(?1..?9, &{:not, &1})) |> generate()
      end
    end
  end

  test "utf_char" do
    assert eventually?(fn -> utf8_char([?á, ?é]) |> generate() == "á" end)
    assert eventually?(fn -> utf8_char([?á, ?é]) |> generate() == "é" end)
  end

  test "string" do
    assert string("foo") |> generate() == "foo"
  end

  test "concat" do
    assert string("foo") |> string("bar") |> generate() == "foobar"
  end

  test "eos" do
    assert string("foo") |> string("bar") |> eos() |> generate() == "foobar"
  end

  test "label" do
    assert string("foo") |> label("foo") |> string("bar") |> generate() == "foobar"
  end

  test "traverse" do
    assert string("foo") |> pre_traverse({:fun, []}) |> string("bar") |> generate() == "foobar"
  end

  test "choice" do
    parsec = choice([string("foo"), string("bar")])
    assert eventually?(fn -> generate(parsec) == "foo" end)
    assert eventually?(fn -> generate(parsec) == "bar" end)

    assert choice(empty(), [string("foo"), string("bar")], gen_weights: [0, 1]) |> generate() ==
             "bar"

    assert choice(empty(), [string("foo"), string("bar")], gen_weights: [1, 0]) |> generate() ==
             "foo"
  end

  test "positive lookahead" do
    assert lookahead(string("foo")) |> string("foo") |> generate() == "foo"
  end

  test "negative lookahead" do
    assert lookahead_not(string("foo")) |> string("bar") |> generate() == "bar"
  end

  test "repeat" do
    parsec = repeat(string("foo"))
    assert eventually?(fn -> generate(parsec) == "" end)
    assert eventually?(fn -> generate(parsec) == "foo" end)
    assert eventually?(fn -> generate(parsec) == "foofoo" end)

    parsec = repeat(empty(), string("foo"), gen_times: 2..3)
    assert eventually?(fn -> generate(parsec) == "foofoo" end)
    assert eventually?(fn -> generate(parsec) == "foofoofoo" end)

    assert repeat(empty(), string("foo"), gen_times: 1) |> generate() == "foo"
  end

  test "times" do
    parsec = times(string("foo"), min: 2, max: 3)
    assert eventually?(fn -> generate(parsec) == "foofoo" end)
    assert eventually?(fn -> generate(parsec) == "foofoofoo" end)

    parsec = times(string("foo"), min: 2, gen_times: 2..3)
    assert eventually?(fn -> generate(parsec) == "foofoofoofoo" end)
    assert eventually?(fn -> generate(parsec) == "foofoofoofoofoo" end)

    assert times(string("foo"), min: 2, gen_times: 3) |> generate() == "foofoofoofoofoo"
  end

  defparsec :string_foo, string("foo"), export_metadata: true
  defparsec :string_choice, choice([parsec(:string_foo), string("bar")]), export_metadata: true

  describe "error conditions" do
    test "parsec" do
      assert_raise RuntimeError, ~r"use a remote parsec instead", fn ->
        parsec(:foo) |> generate()
      end

      assert eventually?(fn -> generate(parsec({__MODULE__, :string_choice})) == "foo" end)
      assert eventually?(fn -> generate(parsec({__MODULE__, :string_choice})) == "bar" end)
    end

    test "eos too early" do
      assert_raise ArgumentError, ~r"found :eos not at the end of parsecs", fn ->
        eos() |> string("foo") |> generate()
      end
    end

    test "unavailable module" do
      assert_raise RuntimeError,
                   "cannot handle parsec({NoSuchModuleSurely, :parse}) because NoSuchModuleSurely is not available",
                   fn ->
                     parsec({NoSuchModuleSurely, :parse}) |> generate()
                   end
    end

    @error "cannot handle parsec({NimbleGeneratorTest, :no_metadata}) because NimbleGeneratorTest did not set :export_metadata when defining no_metadata"
    test "module did not set :export_metadata" do
      assert_raise RuntimeError, @error, fn ->
        parsec({__MODULE__, :no_metadata}) |> generate()
      end
    end

    def no_metadata, do: nil
  end

  defp eventually?(fun) do
    Enum.any?(1..100, fn _ -> fun.() end)
  end
end
