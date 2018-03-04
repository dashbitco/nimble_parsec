# NimbleParsec

<!-- MDOC !-->

`NimbleParsec` is a simple and fast library for parser combinators.

Combinators are built during runtime and compiled into multiple
clauses with binary matching. This provides the following benefits:

  * Performance: since it compiles to binary matching, it leverages
    many Erlang VM optimizations to generate extremely fast parser
    code with low memory usage

  * Composable: this library does not rely on macros for building and
    composing parsers, therefore they are fully composable. The only
    macros are `defparsec/3` and `defparsecp/3` which emit the compiled
    clauses with  binary matching

  * No runtime dependency: after compilation, the generated parser
    clauses have no runtime dependency on `NimbleParsec`. This opens up
    the possibility to compile parsers and do not impose a dependency on
    users of your library

  * No footprints: `NimbleParsec` only needs to be imported in your modules.
    There is no need for `use NimbleParsec`, leaving no footprints on your
    modules

## Examples

```elixir
defmodule MyParser do
  import NimbleParsec

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
    |> optional(string("Z"))

  defparsec :datetime, date |> ignore(string("T")) |> concat(time), debug: true
end

MyParser.datetime("2010-04-17T14:12:34Z")
#=> {:ok, [2010, 4, 17, 14, 12, 34, "Z"], "", %{}, 1, 21}
```

If you add `debug: true` to `defparsec/3`, it will print the generated
clauses, which are shown below:

```elixir
defp datetime__0(<<x0, x1, x2, x3, "-", x4, x5, "-", x6, x7, "T",
                   x8, x9, ":", x10, x11, ":", x12, x13, rest::binary>>,
                 acc, stack, comb__context, comb__line, comb__column)
     when x0 >= 48 and x0 <= 57 and (x1 >= 48 and x1 <= 57) and
         (x2 >= 48 and x2 <= 57) and (x3 >= 48 and x3 <= 57) and
         (x4 >= 48 and x4 <= 57) and (x5 >= 48 and x5 <= 57) and
         (x6 >= 48 and x6 <= 57) and (x7 >= 48 and x7 <= 57) and
         (x8 >= 48 and x8 <= 57) and (x9 >= 48 and x9 <= 57) and
         (x10 >= 48 and x10 <= 57) and (x11 >= 48 and x11 <= 57) and
         (x12 >= 48 and x12 <= 57) and (x13 >= 48 and x13 <= 57) do
  datetime__1(
    rest,
    [(x13 - 48) * 1 + (x12 - 48) * 10, (x11 - 48) * 1 + (x10 - 48) * 10,
     (x9 - 48) * 1 + (x8 - 48) * 10, (x7 - 48) * 1 + (x6 - 48) * 10, (x5 - 48) * 1 + (x4 - 48) * 10,
     (x3 - 48) * 1 + (x2 - 48) * 10 + (x1 - 48) * 100 + (x0 - 48) * 1000] ++ acc,
    stack,
    comb__context,
    comb__line,
    comb__column + 19
  )
end

defp datetime__0(rest, acc, _stack, context, line, column) do
  {:error, "...", rest, context, line, column}
end

defp datetime__1(<<"Z", rest::binary>>, acc, stack, comb__context, comb__line, comb__column) do
  datetime__2(rest, ["Z"] ++ acc, stack, comb__context, comb__line, comb__column + 1)
end

defp datetime__1(rest, acc, stack, context, line, column) do
  datetime__2(rest, acc, stack, context, line, column)
end

defp datetime__2(rest, acc, _stack, context, line, column) do
  {:ok, acc, rest, context, line, column}
end
```

As you can see, it generates highly inlined code, comparable to
hand-written parsers. This gives `NimbleParsec` an order of magnitude
performance gains compared to other parser combinators. Further performance
can be gained by giving the `inline: true` option to `defparsec/3`.

<!-- MDOC !-->

## Installation

Add `nimble_parsec` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:nimble_parsec, "~> 0.1"}]
end
```

## Nimble*

Other nimble libraries by Plataformatec:

  * [NimbleCSV](https://github.com/plataformatec/nimble_csv) - simple and fast CSV parsing

## License

Copyright 2018 Plataformatec

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
