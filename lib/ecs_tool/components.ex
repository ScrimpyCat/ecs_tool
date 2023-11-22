defmodule EcsTool.Components do
    import EcsTool.Formatter, only: [to_macro: 1, format_macro: 2]

    defstruct [archetype: { [], %{} }, packed: { [], %{} }, indexed: { [], %{} }, local: { [], %{} }, names: %{}]

    @type component_type :: :archetype | :packed | :indexed | :local
    @type component_modifier :: :duplicate | :tag | { :destructor, String.t }
    @type name :: String.t
    @type index :: non_neg_integer
    @type t :: %__MODULE__{
        archetype: { [name], %{ index => name } },
        packed: { [name], %{ index => name } },
        indexed: { [name], %{ index => name } },
        local: { [name], %{ index => name } },
        names: %{ name => { component_type, [component_modifier] } }
    }

    @types %{
        "ECS_ARCHETYPE_COMPONENT" => { :archetype, [] },
        "ECS_PACKED_COMPONENT" => { :packed, [] },
        "ECS_INDEXED_COMPONENT" => { :indexed, [] },
        "ECS_LOCAL_COMPONENT" => { :local, [] },
        "ECS_ARCHETYPE_DUPLICATE_COMPONENT" => { :archetype, [:duplicate] },
        "ECS_PACKED_DUPLICATE_COMPONENT" => { :packed, [:duplicate] },
        "ECS_INDEXED_DUPLICATE_COMPONENT" => { :indexed, [:duplicate] },
        "ECS_LOCAL_DUPLICATE_COMPONENT" => { :local, [:duplicate] },
        "ECS_ARCHETYPE_TAG" => { :archetype, [:tag] },
        "ECS_PACKED_TAG" => { :packed, [:tag] },
        "ECS_INDEXED_TAG" => { :indexed, [:tag] },
        "ECS_LOCAL_TAG" => { :local, [:tag] }
    }

    @storage_types [:archetype, :packed, :indexed, :local]

    def extract(components \\ %__MODULE__{}, string, env \\ %{}) do
        env = Enum.map(env, fn { "ECS_ENV(" <> k, v } ->
            { k |> String.trim_trailing(")"), Enum.join(v) }
        end) |> Map.new

        append(Regex.scan(~r/(#{@types |> Map.keys |> Enum.join("|")}|ECS_DESTRUCTOR)\((.*?)\)(?:[^(), +\-*\/!<>%&|]|$)/, string, capture: :all_but_first), components, env)
    end

    defp append([], components, _), do: components
    defp append([["ECS_DESTRUCTOR", args]|t], components = %{ names: names }, env) do
        [destructor|args] = String.split(args, ",")
        destructor = String.trim(destructor)

        names = Enum.reduce(args, names, fn name, acc ->
            name = String.trim(name)
            case acc do
                %{ ^name => { type, modifiers } } -> %{ acc | name => { type, [{ :destructor, destructor }|modifiers] } }
                acc ->
                    IO.puts "\"#{name}\" component does not exist"
                    acc
            end
        end)

        append(t, %{ components | names: names }, env)
    end
    defp append([[type, args]|t], components = %{ names: names }, env) do
        { field, modifiers } = @types[type]
        { unordered, ordered } = Map.get(components, field)

        [name|args] = String.split(args, ",")
        name = String.trim(name)

        components = if Map.has_key?(names, name) do
            IO.puts "\"#{name}\" component already exists"
            components
        else
            { appended, modifiers } = case args do
                [] -> { { [name|unordered], ordered }, modifiers }
                [id] ->
                    id = String.trim(id)
                    { :ok, index } = resolve_id(id, env)

                    modifiers = if String.match?(id, ~r/[a-zA-Z_]/) do
                        [{ :id, id }|modifiers]
                    else
                        modifiers
                    end

                    if Map.has_key?(ordered, index) do
                        IO.puts "#{type} already exists for index (#{index}): replacing \"#{ordered[index]}\" with \"#{name}\""
                    end

                    { { unordered, Map.put(ordered, index, name) }, modifiers }
            end

            %{ components | field => appended, names: Map.put(names, name, { field, modifiers }) }
        end

        append(t, components, env)
    end

    defp resolve_id(id, env, _ \\ "")
    defp resolve_id(id, _, id) do
        case Regex.scan(~r/[a-zA-z_][a-zA-Z0-9_]+/, id) do
            [] ->
                { index, _ } = Code.eval_string(id)
                { :ok, index }
            unresolved ->
                IO.puts "#{IO.ANSI.red}env values missing for names: #{Enum.join(unresolved, ", ")}#{IO.ANSI.default_color}"
                :error
        end
    end
    defp resolve_id(id, env, _), do: resolve_id(String.replace(id, ~r/[a-zA-z_][a-zA-Z0-9_]+/, &(env[&1] || &1)), env, id)

    def kind(components, name), do: components.names[name] |> elem(0)

    def modifiers(components, name), do: components.names[name] |> elem(1)

    def get(components, field) do
        { unordered, ordered } = Map.get(components, field)
        merge(unordered, ordered)
    end

    defp merge(unordered, ordered, n \\ 0, merged \\ [])
    defp merge([], ordered, n, merged) do
        { merged, _ } =
            ordered
            |> Map.to_list
            |> Enum.sort(:asc)
            |> Enum.reduce({ merged, n }, fn
                { index, name }, { acc, n } when index >= n ->
                    [_|acc] = Enum.reduce(0..(index - n), acc, fn _, acc ->
                        [nil|acc]
                    end)
                    { [name|acc], index + 1 }
                { index, _ }, { acc, n } when index < n -> { acc, n }
            end)

        Enum.reverse(merged)
    end
    defp merge(unordered = [h|t], ordered, n, merged) do
        { unordered, merged } = case ordered do
            %{ ^n => name } -> { unordered, [name|merged] }
            _ -> { t, [h|merged] }
        end

        merge(unordered, ordered, n + 1, merged)
    end

    defp unique_modifier_flag(modifier, flag, allowed, unused \\ [])
    defp unique_modifier_flag(modifier, flag, [modifier|allowed], unused), do: { flag, unused }
    defp unique_modifier_flag(modifier, flag, [h|allowed], unused), do: unique_modifier_flag(modifier, flag, allowed, [h|unused])
    defp unique_modifier_flag(_, _, [], unused), do: { "", unused }

    defp modifier_flags(modifiers, allowed \\ [:duplicate, :tag, :destructor], acc \\ [])
    defp modifier_flags([{ :id, _ }|t], allowed, acc), do: modifier_flags(t, allowed, acc)
    defp modifier_flags([:duplicate|t], allowed, acc) do
        { flag, allowed } = unique_modifier_flag(:duplicate, " | ECSComponentStorageModifierDuplicate", allowed)
        modifier_flags([:destructor|t], allowed, [flag|acc])
    end
    defp modifier_flags([{ modifier, _ }|t], allowed, acc), do: modifier_flags([modifier|t], allowed, acc)
    defp modifier_flags([modifier|t], allowed, acc) do
        { flag, allowed } = unique_modifier_flag(modifier, [" | ECSComponentStorageModifier", to_string(modifier) |> String.capitalize], allowed)
        modifier_flags(t, allowed, [flag|acc])
    end
    defp modifier_flags([], _, acc), do: acc

    defp component_id(modifiers, n) do
        case modifiers[:id] do
            nil -> Integer.to_string(n)
            id -> ["(", id, ")"]
        end
    end

    def defines(components, _namespace, local_max \\ nil) do
        fun = fn
            nil, { defs, n, type, names } -> { defs, n + 1, type, names }
            name, { defs, n, type, names } ->
                mods = modifiers(components, name)
                flags = modifier_flags(mods)

                { [defs, ["#define ", to_macro(name), " (", type.(names), flags, " | ", component_id(mods, n), ")\n"]], n + 1, type, [name|names] }
        end

        Enum.map(@storage_types, fn
            kind ->
                names = get(components, kind)
                type = ["ECSComponentStorageType", to_string(kind) |> String.capitalize]
                type = case kind do
                    :local ->
                        index_bits = (local_max || names |> Enum.count) |> Itsy.Bit.mask |> Itsy.Bit.count |> to_string
                        fn names ->
                            offset = case names do
                                [] -> "0"
                                [h|_] -> ["(((", to_macro(h), " & ~ECSComponentStorageMask) >> ", index_bits, ") + sizeof(", if(:duplicate in modifiers(components, h), do: "CCArray", else: h), "))"]
                            end
                            [type, " | (", offset, " << ", index_bits, ")"]
                        end
                    _ -> fn _ -> type end
                end

                names |> Enum.reduce({ [], 0, type, [] }, fun) |> elem(0)
        end)
    end

    def local_storage_size(components, namespace, local_max \\ nil) do
        { count, name } = get(components, :local) |> Enum.reduce({ 0, nil }, fn name, { n, _ } -> { n + 1, name } end)

        size = case count do
            0 -> "0"
            count ->
                index_bits = (local_max || count) |> Itsy.Bit.mask |> Itsy.Bit.count |> to_string
                ["(((", to_macro(name), " & ~ECSComponentStorageMask) >> ", index_bits, ") + sizeof(", if(:duplicate in modifiers(components, name), do: "CCArray", else: name), "))"]
        end

        ["#define ", format_macro("LOCAL_STORAGE_SIZE", namespace), " ", size, "\n"]
    end

    def filter(list, components, type) do
        { names, _ } = Map.get(components, type)
        set = MapSet.new(names)

        Enum.filter(list, &(MapSet.member?(set, &1)))
    end

    def gen_sets([]), do: []
    def gen_sets([h|t]), do: MapSet.new |> gen_sets([h], t) |> MapSet.to_list

    def gen_sets(sets, set, []), do: MapSet.put(sets, Enum.reverse(set))
    def gen_sets(sets, set, [h|t]), do: sets |> gen_sets(set, t) |> gen_sets([h|set], t) |> gen_sets([h], t)

    defp to_arch_indexes(set, comps), do: Enum.map(set, fn x -> Enum.find_index(comps, &match?(^x, &1)) end)

    defp archetype_sets(comps) do
        gen_sets(comps)
        |> Enum.map(&({ &1, { Enum.count(&1), to_arch_indexes(&1, comps) } }))
        |> Enum.sort(fn
            { _, a }, { _, b } -> a > b
        end)
        |> Enum.map(fn { set, _ } -> set end)
    end

    defp format_as_index(components, x) do
        { l, r } = case modifiers(components, x) do
            [] -> { "", "" }
            [{ :id, _ }] -> { "", "" }
            _ -> { "(", " & ~ECSComponentStorageMask)" }
        end

        [l, to_macro(x), r]
    end

    def archetype_deps(components, namespace, relative \\ false, allowed_archs \\ nil) do
        comps = get(components, :archetype)

        count = Enum.count(comps)
        sets = archetype_sets(comps)

        { code, _ } = Enum.reduce(if(allowed_archs, do: Enum.filter(sets, &MapSet.member?(allowed_archs, Enum.sort(&1))), else: sets), { [], { 0, "" } }, fn set, { code, { n, previous } } ->
            pointers = Enum.reduce(sets, [], fn group, acc ->
                indexed_set = Enum.map(set, &Enum.find_index(group, fn x -> x == &1 end))
                if Enum.all?(indexed_set) do
                    size = Enum.count(group) |> to_string
                    [["    ", "{ offsetof(ECSContext, archetypes", size, "[ECS_ARCHETYPE", size, "_INDEX(", Enum.map(group, &format_as_index(components, &1)) |> Enum.join(", "), ")]), ", format_macro("INDEX", namespace), Enum.map(indexed_set, &(["_", to_string(&1)])), " },\n"]|acc]
                else
                    acc
                end
            end)

            current = [format_macro("ARCHETYPE_DEPENDENCIES", namespace), Enum.map(Enum.sort(set), &(["_", &1]))]

            code = [
                code,
                "#define ", current, " ", if(relative, do: ["(", previous, " + ", to_string(n), ")"], else: to_string(n)), "\n",
                pointers
            ]

            { code, { Enum.count(pointers) + if(relative, do: 0, else: n), current } }
        end)

        deps = [
            "const ECSArchetypePointer ", namespace, "ArchetypeDependencies[] = {\n",
            code,
            "};\n"
        ]

        counts = Enum.map(1..count, &(["#define ", format_macro("ARCHETYPE", namespace), to_string(&1), "_DEPENDENCIES_COUNT ", to_string(2**(count - &1)), "\n"]))

        access = Enum.map(1..count, &(["#define ", format_macro("COMPONENT_SYSTEM_ACCESS_ARCHETYPE", namespace), to_string(&1), "(", Enum.map(1..&1, fn x -> "x" <> to_string(x) end) |> Enum.join(", "), ")", " { .count = ", format_macro("ARCHETYPE", namespace), to_string(&1), "_DEPENDENCIES_COUNT, .pointer = &", namespace, "ArchetypeDependencies[", format_macro("ARCHETYPE_DEPENDENCIES", namespace), "_##", Enum.map(1..&1, fn x -> "x" <> to_string(x) end) |> Enum.join("##_##"), "] }\n"]))

        { deps, counts, access }
    end

    def component_sizes(components, namespace) do
        Enum.map(@storage_types, fn v ->
            { sizes, dup_sizes } = get(components, v) |> Enum.reduce({ [], [] }, fn
                nil, { size_acc, dup_acc } -> { ["    0,\n"|size_acc], ["    0,\n"|dup_acc] }
                comp, { size_acc, dup_acc } ->
                    comp_size = ["    sizeof(", comp, "),\n"]
                    if :duplicate in modifiers(components, comp) do
                        { ["    sizeof(CCArray),\n"|size_acc], [comp_size|dup_acc] }
                    else
                        { [comp_size|size_acc], ["    0,\n"|dup_acc] }
                    end
            end)

            max = ["ECS_", to_string(v) |> String.upcase, "_COMPONENT_MAX"]

            [
                "const size_t ", namespace, String.capitalize(to_string(v)), "ComponentSizes[", max, "] = {\n",
                Enum.reverse(sizes),
                "};\n",
                "const size_t ", namespace, "Duplicate", String.capitalize(to_string(v)), "ComponentSizes[", max, "] = {\n",
                Enum.reverse(dup_sizes),
                "};\n"
            ]
        end)
    end

    def component_destructors(components, namespace) do
        Enum.map(@storage_types, fn v ->
            { destructors, dup_destructors } = get(components, v) |> Enum.reduce({ [], [] }, fn
                nil, { destructor_acc, dup_acc } -> { ["    NULL,\n"|destructor_acc], ["    NULL,\n"|dup_acc] }
                comp, { destructor_acc, dup_acc } ->
                    mods = modifiers(components, comp)

                    comp_destructor = case mods[:destructor] do
                        nil -> "    NULL,\n"
                        destructor -> ["    ", destructor, ",\n"]
                    end

                    if :duplicate in mods do
                        { ["    ECSDuplicateDestructor,\n"|destructor_acc], [comp_destructor|dup_acc] }
                    else
                        { [comp_destructor|destructor_acc], ["    NULL,\n"|dup_acc] }
                    end
            end)

            max = ["ECS_", to_string(v) |> String.upcase, "_COMPONENT_MAX"]

            [
                "const ECSComponentDestructor ", namespace, String.capitalize(to_string(v)), "ComponentDestructors[", max, "] = {\n",
                Enum.reverse(destructors),
                "};\n",
                "const ECSComponentDestructor ", namespace, "Duplicate", String.capitalize(to_string(v)), "ComponentDestructors[", max, "] = {\n",
                Enum.reverse(dup_destructors),
                "};\n"
            ]
        end)
    end

    def component_lookup(components, namespace) do
        ids = Enum.map(@storage_types, fn v ->
            get(components, v) |> Enum.map(fn
                nil -> ""
                comp -> ["    [ECS_COMPONENT_BASE_INDEX(", to_macro(comp), ")] = ", to_macro(comp), ",\n"]
            end)
        end)

        [
            "const ECSComponentID ", namespace, "ComponentIDs[ECS_COMPONENT_MAX] = {\n",
            ids,
            "};\n"
        ]
    end

    defp get_indexes(list, a, b), do: { Enum.find_index(list, &match?(^a, &1)), Enum.find_index(list, &match?(^b, &1)) }

    def sort(components, comps) do
        Enum.sort(comps, fn a, b ->
            with { nil, nil } <- get(components, :archetype) |> get_indexes(a, b),
                 { nil, nil } <- get(components, :packed) |> get_indexes(a, b),
                 { nil, nil } <- get(components, :indexed) |> get_indexes(a, b) do
                true
            else
                { nil, _ } -> false
                { _, nil } -> true
                { a_index, b_index } -> a_index < b_index
            end
        end)
    end
end
