defmodule NimbleParsec.IntegrationTest do
  use ExUnit.Case, async: true

  import NimbleParsec

  describe "markdown heading" do
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

  describe "string with quotes inside, using repeat_until" do
    defparsec :string_with_quotes_using_repeat_until,
              ascii_char([?"])
              |> repeat_until(
                choice([
                  ~S(\") |> string() |> replace(?"),
                  utf8_char([])
                ]),
                [ascii_char([?"])]
              )
              |> ascii_char([?"])
              |> reduce({List, :to_string, []})

    test "returns ok/error" do
      assert string_with_quotes_using_repeat_until(~S("string with quotes \" inside")) ==
               {:ok, ["\"string with quotes \" inside\""], "", %{}, {1, 0}, 30}
    end
  end

  describe "signed int" do
    defparsec :signed_int,
              optional(ascii_char([?-]))
              |> integer(min: 1)
              |> traverse({:sign_int_value, []})
              |> tag(:signed_int)

    defp sign_int_value(_rest, [int, _neg], context, _, _) do
      {[int * -1], context}
    end

    defp sign_int_value(_rest, res, context, _, _) do
      {res, context}
    end

    test "returns ok/error" do
      assert signed_int("-1") == {:ok, [signed_int: [-1]], "", %{}, {1, 0}, 2}
    end
  end
end
