defmodule Exhub.MixProject do
  use Mix.Project

  def project do
    [
      app: :exhub,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: [
        exhub: [
          strip_beams: Mix.env() == :prod,
          overwrite: true
        ]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :tzdata, :erlexec],
      mod: {Exhub.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:mdex, "~> 0.14.6"},
      {:mdex_mermex, "~> 0.1.2"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.7"},
      {:plug_socket, "~> 0.1"},
      {:castore, "~> 1.0"},
      {:httpoison, "~> 2.0"},
      {:floki, "~> 0.37.0"},
      {:reverse_proxy_plug, "~> 3.0"},
      {:anubis_mcp, "~> 1.14.0"},
      {:langchain, ">= 0.8.11"},
      {:tzdata, "~> 1.1"},
      {:req, "~> 0.5.8", override: true},
      {:mint_web_socket, "~> 1.0"},
      {:quantum, "~> 3.0"},
      {:secret_vault, "~> 1.0"},
      {:exile, "~> 0.10"},
      {:erlexec, "~> 2.0"},
      {:toon, "~> 0.3.0"},
      {:lua, "~> 1.0"},
      {:ex_mcp, "~> 0.9.0"},
      {:sagents, "~> 0.9.0"},
      {:yamerl, "~> 0.10.0"},
      {:exqlite, "~> 0.39.0"},
      {:sqlite_vec, "~> 0.1.0"},
      # Undeclared transitive dep until `mix deps.unlock --unused` pruned it:
      # `UUID.uuid4/0` is used by the router, so declare it outright.
      {:uuid, "~> 1.1"}
    ]
  end
end
