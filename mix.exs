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
      extra_applications: [:logger],
      mod: {Exhub.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:mdex, "~> 0.2"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.7"},
      {:plug_socket, "~> 0.1"},
      {:castore, "~> 1.0"},
      {:httpoison, "~> 2.0"},
      {:floki, "~> 0.37.0"},
      {:reverse_proxy_plug, "~> 3.0"},
      {:hermes_mcp, git: "https://github.com/zoedsoupe/hermes-mcp", ref: "6a3134ae3288d23b333aea7e7467e6ea93a0ba11"},
      {:swarm_ex, git: "https://github.com/edmondfrank/swarm_ex", ref: "e563429a7173ae0c5fc7d3a955601930ad343e4f"},
      {:instructor, git: "https://github.com/thmsmlr/instructor_ex", ref: "2c89b1cd52e0125562c8c5cb2eb99d2324261cfa"},
      {:langchain, git: "https://github.com/brainlid/langchain", ref: "8237816315ef6333bdde4848810bde13f847b70d"},
      {:req, "~> 0.5.8", override: true}
    ]
  end
end
