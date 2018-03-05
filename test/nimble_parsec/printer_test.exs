defmodule NimbleParsec.PrinterTest do
  use ExUnit.Case, async: true

  test "defparsec/3" do
    print("""
    defmodule Foo do
      <%= defparsec :parse, integer(2) %>
    end
    """)

    {:ok, [42], _, _, _, _} = Foo.parse("42")
    {:error, _, _, _, _, _} = Foo.parse("4")
  after
    purge([Foo])
  end

  test "defparsecp/3" do
    print("""
    defmodule Bar do
      <%= defparsecp :exact_integer, integer(2) %>

      def parse(binary) do
        exact_integer__0(binary, [], [], [], {1, 0}, 0)
      end
    end
    """)

    assert Bar.__info__(:functions) == [parse: 1]

    {:ok, [42], _, _, _, _} = Bar.parse("42")
    {:error, _, _, _, _, _} = Bar.parse("4")
  after
    purge([Bar])
  end

  defp print(code) do
    code
    |> NimbleParsec.Printer.print_string()
    |> Code.compile_string()
  end

  defp purge(modules) do
    for module <- modules do
      :code.purge(module)
      :code.delete(module)
    end
  end
end
