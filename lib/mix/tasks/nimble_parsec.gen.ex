defmodule Mix.Tasks.NimbleParsec.Gen do
  @usage "mix nimble_parsec.gen template1.ex.eex template2.ex.eex ..."

  @shortdoc "Generates parsers from EEx templates"

  @moduledoc """
  Generates parsers from EEx templates.

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

      mix nimble_parsec.gen lib/my_parser.ex.eex

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

  In the template the special variants of `defparsec` and `defparsecp`
  (coming from `NimbleParsec.Printer`) as well as all remaining functions from
  `NimbleParsec` are automatically imported.
  """

  use Mix.Task

  def run(args) do
    Mix.Task.run("loadpaths")

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
    comment = "# Generated from #{source_path}, do not edit.\n\n"
    code = NimbleParsec.Printer.print_file(source_path, [], file: target_path)

    File.write!(target_path, comment <> code)
  end
end
