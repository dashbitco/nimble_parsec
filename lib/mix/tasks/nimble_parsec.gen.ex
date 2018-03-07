defmodule Mix.Tasks.NimbleParsec.Compile do
  @usage "mix nimble_parsec.compile template1.ex.eex template2.ex.eex ..."

  @shortdoc "Compiles parsers from EEx templates"

  @moduledoc ~S"""
  Compiles parsers from EEx templates.

      #{@usage}

  This task is useful to generate parsers that have no runtime dependency
  on NimbleParsec.

  ## Examples

  Let's define a template file:

      # lib/my_parser.ex.eex
      defmodule MyParser do
        @moduledoc false

        <%=
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

          defparsec :datetime, date |> ignore(string("T")) |> concat(time)
        %>
      end

  After running:

      mix nimble_parsec.compile lib/my_parser.ex.eex

  The following file will be generated:

      # lib/my_parser.ex
      defmodule MyParser do
        @moduledoc false

        def datetime(binary, opts \\ []) do
          ...
        end

        defp datetime__0(...) do
          ...
        end

        ...
      end

  The template imports all of `NimbleParsec` functions by default but
  replaces `defparsec` and `defparsecp` by variants coming from
  `NimbleParsec.Printer`.
  """

  use Mix.Task

  def run(args) do
    Mix.Task.run("compile")

    case args do
      [] ->
        Mix.raise("Usage: #{@usage}")

      source_paths ->
        Enum.each(source_paths, &generate/1)
    end
  end

  defp generate(source_path) do
    target_path = Path.rootname(source_path, ".eex")

    Mix.shell().info("Generating #{target_path}")
    from = "# Generated from #{source_path}, do not edit.\n"

    date =
      "# Generated at #{DateTime.utc_now() |> Map.put(:microsecond, {0, 0}) |> to_string}\n\n"

    code = NimbleParsec.Printer.print_file(source_path)

    File.write!(target_path, [from, date | code])
  end
end
