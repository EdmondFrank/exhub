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
      {:hermes_mcp, git: "https://github.com/zoedsoupe/hermes-mcp", ref: "48e3e9baf11552ac70a0761ac967bcff08b6c93a"},
      {:reverse_proxy_plug, "~> 3.0"},
      {:swarm_ex, git: "https://github.com/edmondfrank/swarm_ex", ref: "c6a6ea7e45e57c75c70d6b6c1fd54337af202307"},
      {:instructor, git: "https://github.com/thmsmlr/instructor_ex", ref: "71b25956c2d014fb3a5e1207c1a339c28af1b596"},
      {:langchain, git: "https://github.com/brainlid/langchain", ref: "8237816315ef6333bdde4848810bde13f847b70d"},
      {:req, "~> 0.5.8", override: true}
    ]
  end
end
