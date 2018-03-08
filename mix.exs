defmodule NimbleParsec.MixProject do
  use Mix.Project

  @version "0.2.2"

  def project do
    [
      app: :nimble_parsec,
      version: @version,
      elixir: "~> 1.4",
      name: "NimbleParsec",
      description: "A simple and fast binary parser combinator library",
      deps: deps(),
      docs: docs(),
      package: package()
    ]
  end

  def application do
    []
  end

  defp deps do
    [{:ex_doc, "~> 0.18", only: :docs}]
  end

  defp docs do
    [
      main: "NimbleParsec",
      source_ref: "v#{@version}",
      source_url: "https://github.com/plataformatec/nimble_parsec"
    ]
  end

  defp package do
    %{
      licenses: ["Apache 2"],
      maintainers: ["José Valim"],
      links: %{"GitHub" => "https://github.com/plataformatec/nimble_parsec"}
    }
  end
end
