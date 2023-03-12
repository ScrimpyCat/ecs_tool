defmodule EcsTool do
    def setup(inputs, output, opts \\ []) do
        namespace = opts[:namespace] || ""
        relative = opts[:relative_indexing] || false
        max_arch = opts[:min_arch] || 0
        write = opts[:write] || [:archetype_indexes, :components, :systems, :groups]
        filter_indexes = opts[:filter_indexes] || false

        { components, systems, groups } = extract(inputs)
        groups = EcsTool.Group.sort_systems(groups, systems)

        filtered_set =
            Enum.reduce(groups, [], fn { _, %{ priorities: priorities } }, acc ->
                Enum.reduce(priorities, acc, fn %{ systems: names }, acc ->
                    Enum.reduce(names, acc, fn name, acc ->
                        %{ read: read, write: write } = systems[name]
                        [Enum.sort(read ++ write)|acc]
                    end)
                end)
            end)

        { :ok, out } = File.open(output, [:write])

        if Enum.find(write, &match?(:archetype_indexes, &1)) do
            { indexes, defines } =
                EcsTool.Components.get(components, :archetype)
                |> Enum.count
                |> case do
                    count when count > max_arch -> count - 1
                    _ -> max_arch
                end
                |> EcsTool.Index.series
                |> EcsTool.Index.group(namespace)

            IO.puts(out, indexes)
            IO.puts(out, defines)
        end

        if Enum.find(write, &match?(:components, &1)) do
            IO.puts(out, EcsTool.Components.defines(components, namespace))

            { deps, counts, access } = EcsTool.Components.archetype_deps(components, namespace, relative, MapSet.new(filtered_set))
            IO.puts(out, deps)
            IO.puts(out, counts)
            IO.puts(out, access)
        end

        if Enum.find(write, &match?(:systems, &1)) do
            { defines, code } = EcsTool.System.id_list(systems, namespace)
            IO.puts(out, code)
            IO.puts(out, defines)
        end

        if Enum.find(write, &match?(:groups, &1)) do
            IO.puts(out, EcsTool.Group.defines(groups))
            IO.puts(out, EcsTool.Group.dependencies(groups, namespace))
            IO.puts(out, EcsTool.Group.system_range(groups, namespace))
            IO.puts(out, EcsTool.Group.system_update(groups, systems, namespace))
            IO.puts(out, EcsTool.Group.system_access(groups, systems, namespace))
            IO.puts(out, EcsTool.Group.system_graph(groups, systems, namespace))
            IO.puts(out, EcsTool.Group.groups(groups, namespace))
        end

        File.close(out)
    end

    defp extract(inputs, state \\ { %EcsTool.Components{}, %{}, %{} })
    defp extract([h|t], state) when is_list(h), do: extract(t, extract(h, state))
    defp extract([h|t], { components, systems, groups }) do
        source = File.read!(h)
        extract(t, { EcsTool.Components.extract(components, source), EcsTool.System.extract(systems, source), EcsTool.Group.extract(groups, source) })
    end
    defp extract([], state), do: state

    def config(n) do
        EcsTool.Config.defines(n) |> IO.puts
    end
end
