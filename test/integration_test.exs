defmodule NimbleParsec.IntegrationTest do
  use ExUnit.Case, async: true

  import NimbleParsec

  describe "markdown heading" do
    @doc """
    Docs should apply correctly.
    """
    defparsec :markdown_h1,
              string("#")
              |> ignore()
              |> utf8_string([], min: 1)

    test "returns ok/error" do
      assert markdown_h1("# Heading") == {:ok, [" Heading"], "", %{}, {1, 0}, 9}
    end
  end

  describe "iso datetime no timezone" do
    date =
      integer(4)
      |> ignore(string("-"))
      |> integer(2)
      |> ignore(string("-"))
      |> integer(2)

    time =
      integer(2)
      |> ignore(string(":"))
      |> integer(2)
      |> ignore(string(":"))
      |> integer(2)

    defparsec :datetime, date |> ignore(string("T")) |> concat(time), inline: true

    test "returns ok/error" do
      assert datetime("2010-04-17T14:12:34") ==
               {:ok, [2010, 4, 17, 14, 12, 34], "", %{}, {1, 0}, 19}
    end
  end

  describe "string with quotes inside, using repeat_while" do
    defparsec :string_with_quotes_using_repeat_while,
              ascii_char([?"])
              |> repeat_while(
                choice([
                  ~S(\") |> string() |> replace(?"),
                  utf8_char([])
                ]),
                {:not_quote, []}
              )
              |> ascii_char([?"])
              |> reduce({List, :to_string, []})

    defp not_quote(<<?", _::binary>>, context, _, _), do: {:halt, context}
    defp not_quote(_, context, _, _), do: {:cont, context}

    test "returns ok/error" do
      assert string_with_quotes_using_repeat_while(~S("string with quotes \" inside")) ==
               {:ok, ["\"string with quotes \" inside\""], "", %{}, {1, 0}, 30}
    end
  end

  describe "signed int" do
    defparsec :signed_int,
              optional(ascii_char([?-]))
              |> integer(min: 1)
              |> post_traverse({:sign_int_value, []})
              |> tag(:signed_int)

    defp sign_int_value(rest, [int, _neg], context, _, _) do
      {rest, [int * -1], context}
    end

    defp sign_int_value(rest, res, context, _, _) do
      {rest, res, context}
    end

    test "returns ok/error" do
      assert signed_int("-1") == {:ok, [signed_int: [-1]], "", %{}, {1, 0}, 2}
    end
  end

  describe "language code" do
    language_code =
      ascii_string([?a..?z], 2)
      |> post_traverse(:atomize_language_code)
      |> tag(:language)

    defparsec :language_code, language_code

    defp atomize_language_code(rest, [language_code], context, _line, _offset) do
      {rest, [String.to_atom(language_code)], context}
    end

    test "returns ok/error" do
      assert language_code("pt") == {:ok, [language: [:pt]], "", %{}, {1, 0}, 2}
    end
  end
end
