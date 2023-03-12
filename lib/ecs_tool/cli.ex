defmodule EcsTool.CLI do
    @moduledoc """
      EcsTool is a utility for automating the configuration of the ECS.

      usage:
        ecs_tool [command] [args]

      Commands:
      * `setup` - Setup the ECS.
      * `config` - Print the output for the ECSConfig.
      * `help` - Prints this message and exits.

      ### setup
        ecs_tool setup [options] FILE

      Options:
      * `--input`, `-i FILE` - Add an input file to read from. If the file is a directory then it finds all `*.c` and `*.h` files within it.
      Defaults to finding all `*.c` or `*.h` files in the working directory.
      * `--namespace`, `-n NAMESPACE` - Set the namespace to be used. Defaults to no namespace.
      * `--write`, `-w TYPE` - Set what to write, the allowed types are: `archetype_indexes`, `components`, `systems`, `groups`. Defaults to
      all of them.
      * `--min-arch`, `-m MIN` - Set the minimum archetype size. Defaults to `0`.
      * `--relative-indexing`, `-r` - Enable relative indexing.
      * `--filter-indexes`, `-fi` - Enable index filtering for archetypes.

      ### config
        ecs_tool config arch_count
    """

    def main(args \\ [])
    def main(["help"|_]), do: help()
    def main(["setup"|args]), do: setup(args)
    def main(["config"|args]), do: config(args)
    def main(_), do: help()

    def setup(args, opts \\ [])
    def setup([cmd, file|args], opts) when cmd in ["-i", "--input"], do: setup(args, [{ :input, file }|opts])
    def setup([cmd, namespace|args], opts) when cmd in ["-n", "--namespace"], do: setup(args, [{ :namespace, namespace }|opts])
    def setup([cmd, type|args], opts) when cmd in ["-w", "--write"], do: setup(args, merge_write_types(type, opts))
    def setup([cmd, min|args], opts) when cmd in ["-m", "--min-arch"], do: setup(args, [{ :min_arch, to_integer(min) }|opts])
    def setup([cmd|args], opts) when cmd in ["-r", "--relative-indexing"], do: setup(args, [{ :relative_indexing, true }|opts])
    def setup([cmd|args], opts) when cmd in ["-fi", "--filter-indexes"], do: setup(args, [{ :filter_indexes, true }|opts])
    def setup([file], opts) do
        Enum.reduce(opts, [], fn
            { :input, input }, acc -> [{ input, File.dir?(input) }|acc]
            _, acc -> acc
        end)
        |> case do
            [] -> [{ "**", true }]
            inputs -> inputs
        end
        |> Enum.reduce([], fn
            { dir, true }, acc -> [Path.join(dir, "*.{c,h}") |> Path.wildcard|acc]
            { file, false }, acc -> [file|acc]
        end)
        |> EcsTool.setup(file, opts)
    end
    def setup(_, _), do: help()

    def config(args, opts \\ [])
    def config([count], opts) do
        EcsTool.config(to_integer(count))
    end
    def config(_, _), do: help()

    defp help(), do: get_docs() |> SimpleMarkdown.convert(render: &SimpleMarkdownExtensionCLI.Formatter.format/1) |> IO.puts

    defp get_docs() do
        if Version.match?(System.version, "> 1.7.0") do
            { :docs_v1, _, :elixir, "text/markdown", %{ "en" => doc }, _, _ } = Code.fetch_docs(__MODULE__)
            doc
        else
            { _, doc } = Code.get_docs(__MODULE__, :moduledoc)
            doc
        end
    end

    defp to_write_type("archetype_indexes"), do: :archetype_indexes
    defp to_write_type("components"), do: :components
    defp to_write_type("systems"), do: :systems
    defp to_write_type("groups"), do: :groups
    defp to_write_type(_), do: nil

    defp merge_write_types(type, list) do
        type
        |> String.downcase
        |> to_write_type
        |> case do
            nil -> list
            sym -> [{ :write, [sym|(list[:write] || [])] }|list]
        end
    end

    defp to_integer(value) do
        { value, _ } = Integer.parse(value)
        value
    end
end
