defmodule NimbleParsec.PrinterTest do
  use ExUnit.Case, async: true

  test "defparsec/3" do
    print("""
    defmodule NimbleParsec.PrinterTest.Foo do
      <%= defparsec :parse, integer(2) %>
    end
    """)

    {:ok, [42], _, _, _, _} = NimbleParsec.PrinterTest.Foo.parse("42")
    {:error, _, _, _, _, _} = NimbleParsec.PrinterTest.Foo.parse("4")
  end

  test "defparsecp/3" do
    print("""
    defmodule NimbleParsec.PrinterTest.Bar do
      <%= defparsecp :exact_integer, integer(2) %>

      def parse(binary) do
        exact_integer__0(binary, [], [], [], {1, 0}, 0)
      end
    end
    """)

    assert NimbleParsec.PrinterTest.Bar.__info__(:functions) == [parse: 1]

    {:ok, [42], _, _, _, _} = NimbleParsec.PrinterTest.Bar.parse("42")
    {:error, _, _, _, _, _} = NimbleParsec.PrinterTest.Bar.parse("4")
  end

  defp print(code) do
    code
    |> NimbleParsec.Printer.print_string()
    |> IO.iodata_to_binary()
    |> Code.compile_string()
  end
end
