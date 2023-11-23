defmodule EcsTool.System do
    import EcsTool.Formatter, only: [to_macro: 1, format_macro: 2]

    defstruct [name: nil, parallel: false, read: [], write: []]

    @type name :: String.t
    @type component :: name
    @type parallel :: { :archetype | component, size :: String.t }
    @type t :: %__MODULE__{
        name: name | nil,
        parallel: false | parallel,
        read: [component],
        write: [component]
    }
    @type systems :: %{ name => t }

    @types %{
        "ECS_SYSTEM" => false,
        "ECS_PARALLEL_SYSTEM" => true
    }

    def extract(systems \\ %{}, string) do
        append(Regex.scan(~r/(#{@types |> Map.keys |> Enum.join("|")})\((.*?),\s*?\((.*?)\)\s*?,\s*?\((.*?)\)\s*?(,\s*?\((.*?)\)\s*?|,(.*?))?\)(?:[^(),]|$)/, string, capture: :all_but_first), systems)
    end

    defp append([], systems), do: systems
    defp append([[type, name, read, write|args]|t], systems) do
        parallel = @types[type]
        name = String.trim(name)

        systems = if Map.has_key?(systems, name) do
            IO.puts "#{IO.ANSI.red}\"#{name}\" system already exists#{IO.ANSI.default_color}"
            systems
        else
            writes = write |> String.split(",", trim: true) |> Enum.map(&String.trim/1)
            reads = (read |> String.split(",", trim: true) |> Enum.map(&String.trim/1)) -- writes

            Map.put(systems, name, %EcsTool.System{
                name: name,
                parallel: process_parallel_args(parallel, args),
                read: reads,
                write: writes
            })
        end

        append(t, systems)
    end

    defp process_parallel_args(false, _), do: false
    defp process_parallel_args(true, []), do: { :archetype, "SIZE_MAX" }
    defp process_parallel_args(true, [_, args]) do
        args
        |> String.split(",", trim: true)
        |> Enum.map(&String.trim/1)
        |> case do
            [comp, size] -> { comp, size }
            [size] -> { :archetype, size }
        end
    end
    defp process_parallel_args(true, [_, _, size]), do: { :archetype, String.trim(size) }

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

    defp gen_list(set, graph, macro, var, value_fun, defines \\ [], id_list \\ [], skip \\ false, n \\ 0)
    defp gen_list([], _, _, _, _, defines, id_list, _, _), do: { defines, id_list }
    defp gen_list([h|t], graph, macro, var, value_fun, defines, id_list, skip, n) do
        { graph, defines, id_list, merge, n } = if Map.has_key?(graph, h) do
            { graph, defines } = Enum.reduce(graph[h], { graph, defines }, fn node, { graph, defines } ->
                defines = if Map.has_key?(graph, node) do
                    [defines, "#define ", macro, Enum.map(node, &(["_", &1])), " (", var, " + ", to_string(n + index_of_sequence(h, node)), ")\n"]
                else
                    defines
                end
                { Map.delete(graph, node), defines }
            end)

            values = if(skip, do: tl(h), else: h)
            merge = mergeable?(List.last(h), t)

            { graph, defines, [id_list, "    ", Enum.map(values, &([value_fun.(&1), ", "])), "\n"], merge, n + Enum.count(h) - (if merge, do: 1, else: 0) }
        else
            { graph, defines, id_list, false, n }
        end

        gen_list(t, graph, macro, var, value_fun, defines, id_list, merge, n)
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

        { defines, id_list } = gen_list(comps, graph(comps, comps), format_macro("COMPONENT_ID_LIST", namespace), [namespace, "ComponentIDList"], &to_macro/1)

        { defines, ["const ECSComponentID ", namespace, "ComponentIDList[] = {\n", id_list, "};\n"] }
    end

    defp filter_kind(components, kinds, comps), do: Enum.filter(comps, fn c -> EcsTool.Components.kind(components, c) not in kinds end)

    def component_offset_list(systems, components, namespace) do
        comps =
            systems
            |> Map.values
            |> Enum.reduce(MapSet.new, &(&2 |> put(filter_kind(components, [:archetype, :local], &1.read ++ &1.write))))
            |> MapSet.to_list
            |> Enum.sort(:asc)
            |> Enum.sort(&(Enum.count(&1) >= Enum.count(&2)))

        { defines, off_list } = gen_list(comps, graph(comps, comps), format_macro("COMPONENT_OFFSET_LIST", namespace), [namespace, "ComponentOffsetList"], fn comp ->
            case EcsTool.Components.kind(components, comp) do
                :packed -> ["offsetof(ECSContext, packed[(", to_macro(comp), " & ~ECSComponentStorageMask)])"]
                :indexed -> ["offsetof(ECSContext, indexed[(", to_macro(comp), " & ~ECSComponentStorageMask)])"]
            end
        end)

        { defines, ["const size_t ", namespace, "ComponentOffsetList[] = {\n", off_list, "};\n"] }
    end

    def component_accessors(systems, components) do
        Enum.map(systems, fn { name, system } ->
            const_set = MapSet.new(system.read)

            EcsTool.Components.sort(components, system.read ++ system.write)
            |> Enum.reduce({ [], 0, 0 }, fn comp, { defines, arch_index, components_index } ->
                qualifier = if(MapSet.member?(const_set, comp), do: " const", else: "")

                case EcsTool.Components.kind(components, comp) do
                    :archetype -> { [defines, "#define ", name, "_", comp, " ", "ECS_ARCHETYPE_VAR->components[*(ECS_ARCHETYPE_COMPONENT_INDEXES_VAR + ", to_string(arch_index), ")], ECS_ARCHETYPE_VAR->entities,", qualifier, "\n"], arch_index + 1, components_index }
                    :packed -> { [defines, "#define ", name, "_", comp, " ", "*((ECSPackedComponent*)((void*)ECS_CONTEXT_VAR + ECS_COMPONENT_OFFSETS_VAR[", to_string(components_index), "]))->components, ((ECSPackedComponent*)((void*)ECS_CONTEXT_VAR + ECS_COMPONENT_OFFSETS_VAR[", to_string(components_index), "]))->entities,", qualifier, "\n"], arch_index, components_index + 1 }
                    :indexed -> { [defines, "#define ", name, "_", comp, " ", "*(ECSIndexedComponent*)((void*)ECS_CONTEXT_VAR + ECS_COMPONENT_OFFSETS_VAR[", to_string(components_index), "]), NULL,", qualifier, "\n"], arch_index, components_index + 1 }
                    :local -> { [defines, "#define ", name, "_", comp, " ", "NULL, NULL,", qualifier, "\n"], arch_index, components_index }
                end
            end)
            |> elem(0)
        end)
    end

    def component_iterators(systems, components) do
        EcsTool.Components.sort(components, components(systems))
        |> Enum.map(fn comp ->
            type = case EcsTool.Components.kind(components, comp) do
                :archetype -> 0
                :packed -> 1
                :indexed -> 2
                :local -> 3
            end

            { declare, declare_element, declare_index, nested } = if :duplicate in EcsTool.Components.modifiers(components, comp) do
                {
                    "ECS_ITER_DECLARE_ARRAY_VAR(",
                    ["#define ECS_ITER_DECLARE_ELEMENT_", comp, " ", comp, " ECS_ITER_DECLARE_ASSIGN(\n"],
                    ["#define ECS_ITER_DECLARE_ELEMENT_INDEX_", comp, " ECS_ITER_DECLARE_ASSIGN(ECS_ITER_PREPEND(ECS_ITER_DUPLICATE_ARRAY_INDEX_SUFFIX,\n"],
                    "ECS_ITER_NESTED_ARRAY_ITERATOR ECS_ITER_IGNORE("
                }
            else
                {
                    [comp, " ECS_ITER_DECLARE_ASSIGN("],
                    "",
                    "",
                    "ECS_ITER_NESTED_NONE ECS_ITER_IGNORE("
                 }
            end

            [
                "#define ECS_ITER_TYPE_", comp, " ", comp, " ECS_ITER_IGNORE(\n",
                "#define ECS_ITER_KIND_", comp, " ", to_string(type), "\n",
                "#define ECS_ITER_DECLARE_", comp, " ", declare, "\n",
                declare_element,
                declare_index,
                "#define ECS_ITER_NESTED_", comp, " ", nested, "\n",
                "#define ECS_ID_", comp, " ", to_macro(comp), "\n"
            ]
        end)
    end

    defp parallel_assertion(parallel, system, components, comp \\ nil)
    defp parallel_assertion(false, _, _, _), do: " ECS_ITER_IGNORE\n"
    defp parallel_assertion({ :archetype, _ }, _, _, _), do: "(...) ECS_ITER_ASSERT_KIND(0, __VA_ARGS__)\n"
    defp parallel_assertion({ comp, size }, system, components, nil), do: parallel_assertion({ EcsTool.Components.kind(components, comp), size }, system, components, comp)
    defp parallel_assertion(_, system, components, comp) do
        [
            " ECS_ITER_ASSERT_TYPE\n",
            EcsTool.Components.sort(components, system.read ++ system.write)
            |> Enum.map(fn
                ^comp -> ["#define ECS_ITER_ASSERT_", system.name, "_", comp, "\n"]
                name -> ["#define ECS_ITER_ASSERT_", system.name, "_", name, " CC_ERROR(\"Must iterate with ", comp, " as the leading component\");\n"]
            end)
        ]
    end

    def assert_iterators(systems, components) do
        Enum.map(systems, fn { name, system } ->
            ["#define ECS_ITER_ASSERT_", name, parallel_assertion(system.parallel, system, components)]
        end)
    end

    def resolve(systems, components, env \\ %{}) do
        Enum.map(systems, fn { name, system } ->
            writes = Enum.reduce(system.write, [], &resolve_component(&1, components, env, &2)) |> Enum.filter(&match?(x when x != "", &1)) |> Enum.uniq
            reads = (Enum.reduce(system.read, [], &resolve_component(&1, components, env, &2)) |> Enum.uniq)  -- [""|writes]

            { name, %{ system | read: reads, write: writes } }
        end)
        |> Map.new
    end

    defp resolve_component(component, components, env, list) do
        Enum.reduce(env[component] || [component], list, &resolve_component_expression(&1, &2, components))
    end

    defp resolve_component_expression("ECS_EVERY_COMPONENT", acc, components), do: include_components(components, acc, { :_, :_ }, true)
    defp resolve_component_expression("ECS_EVERY_ARCHETYPE_COMPONENT", acc, components), do: include_components(components, acc, { :archetype, :_ }, true)
    defp resolve_component_expression("ECS_EVERY_PACKED_COMPONENT", acc, components), do: include_components(components, acc, { :packed, :_ }, true)
    defp resolve_component_expression("ECS_EVERY_INDEXED_COMPONENT", acc, components), do: include_components(components, acc, { :indexed, :_ }, true)
    defp resolve_component_expression("ECS_EVERY_LOCAL_COMPONENT", acc, components), do: include_components(components, acc, { :local, :_ }, true)
    defp resolve_component_expression("ECS_EVERY_NORMAL_COMPONENT", acc, components), do: include_components(components, acc, { :_, [] }, true)
    defp resolve_component_expression("ECS_EVERY_NORMAL_ARCHETYPE_COMPONENT", acc, components), do: include_components(components, acc, { :archetype, [] }, true)
    defp resolve_component_expression("ECS_EVERY_NORMAL_PACKED_COMPONENT", acc, components), do: include_components(components, acc, { :packed, [] }, true)
    defp resolve_component_expression("ECS_EVERY_NORMAL_INDEXED_COMPONENT", acc, components), do: include_components(components, acc, { :indexed, [] }, true)
    defp resolve_component_expression("ECS_EVERY_NORMAL_LOCAL_COMPONENT", acc, components), do: include_components(components, acc, { :local, [] }, true)
    defp resolve_component_expression("ECS_EVERY_DUPLICATE_COMPONENT", acc, components), do: include_components(components, acc, { :_, [:duplicate] }, true)
    defp resolve_component_expression("ECS_EVERY_DUPLICATE_ARCHETYPE_COMPONENT", acc, components), do: include_components(components, acc, { :archetype, [:duplicate] }, true)
    defp resolve_component_expression("ECS_EVERY_DUPLICATE_PACKED_COMPONENT", acc, components), do: include_components(components, acc, { :packed, [:duplicate] }, true)
    defp resolve_component_expression("ECS_EVERY_DUPLICATE_INDEXED_COMPONENT", acc, components), do: include_components(components, acc, { :indexed, [:duplicate] }, true)
    defp resolve_component_expression("ECS_EVERY_DUPLICATE_LOCAL_COMPONENT", acc, components), do: include_components(components, acc, { :local, [:duplicate] }, true)
    defp resolve_component_expression("ECS_EVERY_TAG_COMPONENT", acc, components), do: include_components(components, acc, { :_, [:tag] }, true)
    defp resolve_component_expression("ECS_EVERY_TAG_ARCHETYPE_COMPONENT", acc, components), do: include_components(components, acc, { :archetype, [:tag] }, true)
    defp resolve_component_expression("ECS_EVERY_TAG_PACKED_COMPONENT", acc, components), do: include_components(components, acc, { :packed, [:tag] }, true)
    defp resolve_component_expression("ECS_EVERY_TAG_INDEXED_COMPONENT", acc, components), do: include_components(components, acc, { :indexed, [:tag] }, true)
    defp resolve_component_expression("ECS_EVERY_TAG_LOCAL_COMPONENT", acc, components), do: include_components(components, acc, { :local, [:tag] }, true)
    defp resolve_component_expression("ECS_MATCH_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :_, :_ }, match)
    defp resolve_component_expression("ECS_MATCH_ARCHETYPE_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :archetype, :_ }, match)
    defp resolve_component_expression("ECS_MATCH_PACKED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :packed, :_ }, match)
    defp resolve_component_expression("ECS_MATCH_INDEXED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :indexed, :_ }, match)
    defp resolve_component_expression("ECS_MATCH_LOCAL_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :local, :_ }, match)
    defp resolve_component_expression("ECS_MATCH_NORMAL_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :_, [] }, match)
    defp resolve_component_expression("ECS_MATCH_NORMAL_ARCHETYPE_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :archetype, [] }, match)
    defp resolve_component_expression("ECS_MATCH_NORMAL_PACKED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :packed, [] }, match)
    defp resolve_component_expression("ECS_MATCH_NORMAL_INDEXED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :indexed, [] }, match)
    defp resolve_component_expression("ECS_MATCH_NORMAL_LOCAL_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :local, [] }, match)
    defp resolve_component_expression("ECS_MATCH_DUPLICATE_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :_, [:duplicate] }, match)
    defp resolve_component_expression("ECS_MATCH_DUPLICATE_ARCHETYPE_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :archetype, [:duplicate] }, match)
    defp resolve_component_expression("ECS_MATCH_DUPLICATE_PACKED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :packed, [:duplicate] }, match)
    defp resolve_component_expression("ECS_MATCH_DUPLICATE_INDEXED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :indexed, [:duplicate] }, match)
    defp resolve_component_expression("ECS_MATCH_DUPLICATE_LOCAL_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :local, [:duplicate] }, match)
    defp resolve_component_expression("ECS_MATCH_TAG_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :_, [:tag] }, match)
    defp resolve_component_expression("ECS_MATCH_TAG_ARCHETYPE_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :archetype, [:tag] }, match)
    defp resolve_component_expression("ECS_MATCH_TAG_PACKED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :packed, [:tag] }, match)
    defp resolve_component_expression("ECS_MATCH_TAG_INDEXED_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :indexed, [:tag] }, match)
    defp resolve_component_expression("ECS_MATCH_TAG_LOCAL_COMPONENT" <> match, acc, components), do: include_components(components, acc, { :local, [:tag] }, match)
    defp resolve_component_expression(component, acc, _), do: [component|acc]

    defp include_components(components, acc, { :_, :_ }, match) do
        Enum.reduce(components.names, acc, fn
            { name, _ }, acc -> if(name_match?(name, match), do: [name|acc], else: acc)
        end)
    end
    defp include_components(components, acc, { :_, mods }, match) do
        Enum.reduce(components.names, acc, fn
            { name, { _, ^mods } }, acc -> if(name_match?(name, match), do: [name|acc], else: acc)
            { name, { _, modifiers } }, acc ->
                if modifiers_match?(modifiers, mods) do
                    if(name_match?(name, match), do: [name|acc], else: acc)
                else
                    acc
                end
            _, acc -> acc
        end)
    end
    defp include_components(components, acc, { type, :_ }, match) do
        Enum.reduce(components.names, acc, fn
            { name, { ^type, _ } }, acc -> if(name_match?(name, match), do: [name|acc], else: acc)
            _, acc -> acc
        end)
    end
    defp include_components(components, acc, { type, mods }, match) do
        Enum.reduce(components.names, acc, fn
            { name, { ^type, ^mods } }, acc -> if(name_match?(name, match), do: [name|acc], else: acc)
            { name, { ^type, modifiers } }, acc ->
                if modifiers_match?(modifiers, mods) do
                    if(name_match?(name, match), do: [name|acc], else: acc)
                else
                    acc
                end
            _, acc -> acc
        end)
    end

    defp modifiers_match?(a, b) do
        Enum.filter(a, fn
            { :destructor, _ } -> false
            _ -> true
        end)
        |> Enum.sort == Enum.sort(b)
    end

    defp name_match?(_, true), do: true
    defp name_match?(name, match), do: Regex.match?(~r/#{match}/, name)
end
