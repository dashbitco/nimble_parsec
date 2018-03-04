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
end
