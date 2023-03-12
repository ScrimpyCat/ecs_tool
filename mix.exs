defmodule EcsTool.MixProject do
    use Mix.Project

    def project do
        [
            app: :ecs_tool,
            description: "A utility for automating the configuration of the ECS",
            version: "0.1.0",
            elixir: "~> 1.14",
            start_permanent: Mix.env() == :prod,
            deps: deps(),
            package: package(),
            escript: escript(),
            dialyzer: [plt_add_deps: :transitive]
        ]
    end

    def application do
        [extra_applications: [:logger]]
    end

    defp deps do
        [
            { :itsy, "~> 0.0.4" },
            { :simple_markdown, "~> 0.8" },
            { :simple_markdown_extension_cli, "~> 0.1.4" },
            { :ex_doc, "~> 0.29", only: :dev }
        ]
    end

    defp package do
        [
            maintainers: ["Stefan Johnson"],
            licenses: ["BSD 2-Clause"],
            links: %{ "GitHub" => "https://github.com/ScrimpyCat/EcsTool" }
        ]
    end

    defp escript do
        [
            main_module: EcsTool.CLI,
            strip_beams: false
        ]
    end
end
