defmodule EcsTool do
    def setup(inputs, output, opts \\ []) do
        namespace = opts[:namespace] || ""
        relative = opts[:relative_indexing] || false
        max_arch = opts[:min_arch] || 0
        write = opts[:write] || [:archetype_indexes, :components, :systems, :groups]
        filter_indexes = opts[:filter_indexes] || false
        accessors_file = opts[:accessors] || nil
        max_local = opts[:max_local] || nil
        env = opts[:env] || %{}

        { components, systems, groups } = extract(inputs) |> validate!(env)
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
            if max_local do
                IO.puts(out, ["_Static_assert(", to_string(max_local), " == ECS_LOCAL_COMPONENT_MAX, \"Regenerate file with new ECS_LOCAL_COMPONENT_MAX value.\");\n"])
            end

            IO.puts(out, EcsTool.Components.defines(components, namespace, max_local))

            IO.puts(out, EcsTool.Components.local_storage_size(components, namespace, max_local))

            IO.puts(out, EcsTool.Components.component_lookup(components, namespace))

            IO.puts(out, EcsTool.Components.component_sizes(components, namespace))

            IO.puts(out, EcsTool.Components.component_destructors(components, namespace))

            { deps, counts, access } = EcsTool.Components.archetype_deps(components, namespace, relative, MapSet.new(filtered_set))
            IO.puts(out, deps)
            IO.puts(out, counts)
            IO.puts(out, access)
        end

        if Enum.find(write, &match?(:systems, &1)) do
            { defines, code } = EcsTool.System.id_list(systems, namespace)
            IO.puts(out, code)
            IO.puts(out, defines)

            { defines, code } = EcsTool.System.component_offset_list(systems, components, namespace)
            IO.puts(out, code)
            IO.puts(out, defines)
        end

        if Enum.find(write, &match?(:groups, &1)) do
            IO.puts(out, EcsTool.Group.defines(groups))
            IO.puts(out, EcsTool.Group.dependencies(groups, namespace))
            IO.puts(out, EcsTool.Group.system_range(groups, namespace))
            IO.puts(out, EcsTool.Group.system_update(groups, systems, components, namespace))
            IO.puts(out, EcsTool.Group.system_access(groups, systems, components, namespace))
            IO.puts(out, EcsTool.Group.system_graph(groups, systems, namespace))
            IO.puts(out, EcsTool.Group.groups(groups, namespace))
        end

        File.close(out)

        if accessors_file do
            { :ok, accessors_out } = File.open(accessors_file, [:write])

            IO.puts(accessors_out, EcsTool.System.component_accessors(systems, components))
            IO.puts(accessors_out, EcsTool.System.component_iterators(systems, components))
            IO.puts(accessors_out, EcsTool.System.assert_iterators(systems, components))

            File.close(accessors_out)
        end
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

    defp validate_key(valid, map, [item|items], error_formatter) do
        if Map.has_key?(map, item) do
            valid
        else
            IO.puts "#{IO.ANSI.red}#{error_formatter.(item)}#{IO.ANSI.default_color}"
            :invalid
        end
        |> validate_key(map, items, error_formatter)
    end
    defp validate_key(valid, _, [], _), do: valid

    defmodule ValidationError do
        defexception message: "Validation failure"
    end

    defp validate!({ components, systems, groups }, env) do
        systems = EcsTool.System.resolve(systems, components, env)

        valid = Enum.reduce(systems, :valid, fn { name, system }, valid ->
            error = &("Error validating system \"#{name}\" #{&1} components: Component \"#{&2}\" does not exist.")

            case system.parallel do
                { comp, _ } when is_binary(comp) -> validate_key(valid, Map.new(system.read ++ system.write, &({ &1, &1 })), [comp], &("Error validating system \"#{name}\" parallelism: Missing read or write access for component \"#{&1}\"."))
                _ -> valid
            end
            |> validate_key(components.names, system.read, &(error.("read", &1)))
            |> validate_key(components.names, system.write, &(error.("write", &1)))
        end)

        valid = Enum.reduce(groups, valid, fn { name, group }, valid ->
            system_error = &("Error validating group \"#{name}\" priority \"#{&1}\" systems: System \"#{&2}\" does not exist.")
            group_error = &("Error validating group \"#{name}\" priority \"#{&1}\" dependencies: Group \"#{&2}\" does not exist.")

            { valid, _ } = Enum.reduce(group.priorities, { valid, 0 }, fn priority, { valid, n } ->
                valid = validate_key(valid, systems, priority.systems, &(system_error.(n, &1)))

                valid = case priority.dependency do
                    { "-1", _ } -> valid
                    { group_name, _ } -> validate_key(valid, groups, [group_name], &(group_error.(n, &1)))
                    _ -> valid
                end

                { valid, n + 1 }
            end)

            valid
        end)

        if valid == :invalid do
            raise ValidationError
        end

        { components, systems, groups }
    end
end
