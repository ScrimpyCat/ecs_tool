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
      Any additional file paths up until the next command or file will be included. Defaults to finding all `*.c` or `*.h` files in the working
      directory.
      * `--namespace`, `-n NAMESPACE` - Set the namespace to be used. Defaults to no namespace.
      * `--write`, `-w TYPE` - Set what to write, the allowed types are: `archetype_indexes`, `components`, `systems`, `groups`. Any additional
      types up until the next command or file will be included. Defaults to all of them. Optionally these can be further broken down by using
      `TYPE:SECTION`. See __Write Sections__ for available options.
      * `--min-arch`, `-m MIN` - Set the minimum archetype size. Defaults to `0`.
      * `--relative-indexing`, `-r` - Enable relative indexing.
      * `--filter-indexes`, `-fi` - Enable index filtering for archetypes.
      * `--accessors`, `-a FILE` - File to write the system component accessors.
      * `--max-local`, `-ml MAX` - Set the maximum number of local components. Defaults to number of local components parsed.
      * `--env`, `-e NAME=VALUE` - Adds a value to the ECS_ENV variable.
      * `--config`, `-c FILE` - Load a set of ECS_ENV variables from the file.

      Write Sections:
      * `components:ids` - Write the component ID defines.
      * `components:env` - Write the component defaults as the env values.
      * `components:data` - Write the component data.
      * `components:deps` - Write the component archetype dependencies.

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
    def setup([cmd, file|args], opts) when cmd in ["-a", "--accessors"], do: setup(args, [{ :accessors, file }|opts])
    def setup([cmd, min|args], opts) when cmd in ["-ml", "--max-local"], do: setup(args, [{ :max_local, to_integer(min) }|opts])
    def setup([cmd, env|args], opts) when cmd in ["-e", "--env"], do: setup(args, merge_env(env, opts))
    def setup([cmd, file|args], opts) when cmd in ["-c", "--config"], do: setup(args, File.stream!(file) |> Enum.reduce(opts, &merge_env(&1, &2)))
    def setup([file|args = [_|_]], opts = [{ :input, _ }|_]), do: setup(args, [{ :input, file }|opts])
    def setup([type|args = [_|_]], opts = [{ :write, _ }|_]), do: setup(args, merge_write_types(type, opts))
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

    defp add_write_type("archetype_indexes", write), do: [:archetype_indexes|write]
    defp add_write_type("components", write), do: [{ :components, :ids }, { :components, :env }, { :components, :data }, { :components, :deps }|write]
    defp add_write_type("components:ids", write), do: [{ :components, :ids }|write]
    defp add_write_type("components:env", write), do: [{ :components, :env }|write]
    defp add_write_type("components:data", write), do: [{ :components, :data }|write]
    defp add_write_type("components:deps", write), do: [{ :components, :deps }|write]
    defp add_write_type("systems", write), do: [:systems|write]
    defp add_write_type("groups", write), do: [:groups|write]
    defp add_write_type(_, write), do: write

    defp merge_write_types(type, list) do
        write = type |> String.downcase |> add_write_type(list[:write] || [])
        [{ :write, write }|list]
    end

    defp to_integer(value) do
        { value, _ } = Integer.parse(value)
        value
    end

    defp merge_env(env, list) do
        envs = list[:env] || %{}

        [name, value] = String.split(env, "=", parts: 2)

        key = "ECS_ENV(" <> String.trim(name) <> ")"

        [{ :env, Map.put(envs, key, [String.trim(value)|(envs[key] || [])]) }|list]
    end
end
