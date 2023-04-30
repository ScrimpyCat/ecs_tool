defmodule EcsTool.System do
    import EcsTool.Formatter, only: [to_macro: 1, format_macro: 2]

    defstruct [name: nil, parallel: false, read: [], write: []]

    @type name :: String.t
    @type component :: name
    @type t :: %__MODULE__{
        name: name | nil,
        parallel: boolean,
        read: [component],
        write: [component]
    }
    @type systems :: %{ name => t }

    @types %{
        "ECS_SYSTEM" => false,
        "ECS_PARALLEL_SYSTEM" => true
    }

    def extract(systems \\ %{}, string) do
        append(Regex.scan(~r/(#{@types |> Map.keys |> Enum.join("|")})\((.*?),.*?\((.*?)\).*?,.*?\((.*?)\).*?\)/, string, capture: :all_but_first), systems)
    end

    defp append([], systems), do: systems
    defp append([[type, name, read, write]|t], systems) do
        parallel = @types[type]
        name = String.trim(name)

        systems = if Map.has_key?(systems, name) do
            IO.puts "\"#{name}\" system already exists"
            systems
        else
            writes = write |> String.split(",", trim: true) |> Enum.map(&String.trim/1)
            reads = (read |> String.split(",", trim: true) |> Enum.map(&String.trim/1)) -- writes

            Map.put(systems, name, %EcsTool.System{
                name: name,
                parallel: parallel,
                read: reads,
                write: writes
            })
        end

        append(t, systems)
    end

    def components(systems) do
        systems
        |> Map.values
        |> Enum.reduce(MapSet.new, &MapSet.union(&2, MapSet.new(&1.read ++ &1.write)))
        |> MapSet.to_list
    end

    defp has_sequence?([], _), do: false
    defp has_sequence?(set, seq), do: match_sequence(set, seq) || has_sequence?(tl(set), seq)

    defp match_sequence(_, []), do: true
    defp match_sequence([h|a], [h|b]), do: match_sequence(a, b)
    defp match_sequence(_, _), do: false

    defp index_of_sequence(set, seq, n \\ 0) do
        if match_sequence(set, seq) do
            n
        else
            index_of_sequence(tl(set), seq, n + 1)
        end
    end

    defp graph(set, nodes, graph \\ %{})
    defp graph(_, [], graph), do: graph
    defp graph(set, [h|t], graph), do: graph(set, t, Map.put(graph, h, Enum.filter(set, &has_sequence?(h, &1))))

    defp mergeable?(x, [[x|_]|_]), do: true
    defp mergeable?(_, _), do: false

    defp gen_id_list(set, graph, namespace, defines \\ [], id_list \\ [], skip \\ false, n \\ 0)
    defp gen_id_list([], _, _, defines, id_list, _, _), do: { defines, id_list }
    defp gen_id_list([h|t], graph, namespace, defines, id_list, skip, n) do
        { graph, defines, id_list, merge, n } = if Map.has_key?(graph, h) do
            { graph, defines } = Enum.reduce(graph[h], { graph, defines }, fn node, { graph, defines } ->
                defines = if Map.has_key?(graph, node) do
                    [defines, "#define ", format_macro("COMPONENT_ID_LIST", namespace), Enum.map(node, &(["_", &1])), " (", namespace,  "ComponentIDList + ", to_string(n + index_of_sequence(h, node)), ")\n"]
                else
                    defines
                end
                { Map.delete(graph, node), defines }
            end)

            values = if(skip, do: tl(h), else: h)
            merge = mergeable?(List.last(h), t)

            { graph, defines, [id_list, "    ", Enum.map(values, &([to_macro(&1), ", "])), "\n"], merge, n + Enum.count(h) - (if merge, do: 1, else: 0) }
        else
            { graph, defines, id_list, false, n }
        end

        gen_id_list(t, graph, namespace, defines, id_list, merge, n)
    end

    defp put(set, []), do: set
    defp put(set, comps), do: MapSet.put(set, Enum.sort(comps))

    def id_list(systems, namespace) do
        comps =
            systems
            |> Map.values
            |> Enum.reduce(MapSet.new, &(&2 |> put(&1.read) |> put(&1.write)))
            |> MapSet.to_list
            |> Enum.sort(:asc)
            |> Enum.sort(&(Enum.count(&1) >= Enum.count(&2)))

        { defines, id_list } = gen_id_list(comps, graph(comps, comps), namespace)

        { defines, ["ECSComponentID ", namespace, "ComponentIDList[] = {\n", id_list, "};\n"] }
    end

    def component_accessors(systems, components) do
        Enum.map(systems, fn { name, system } ->
            EcsTool.Components.sort(components, system.read ++ system.write)
            |> Enum.reduce({ [], 0, 0 }, fn comp, { defines, arch_index, components_index } ->
                case EcsTool.Components.kind(components, comp) do
                    :archetype -> { [defines, "#define ", name, "_", comp, " ", "ECS_ARCHETYPE_VAR->components[*(ECS_ARCHETYPE_COMPONENT_INDEXES_VAR + ", to_string(arch_index), ")], ECS_ARCHETYPE_VAR->entities\n"], arch_index + 1, components_index }
                    :individual -> { [defines, "#define ", name, "_", comp, " ", "*ECS_COMPONENTS_VAR[", to_string(components_index), "].components, ECS_COMPONENTS_VAR[", to_string(components_index), "].entities\n"], arch_index, components_index + 1 }
                end
            end)
            |> elem(0)
        end)
    end
end
