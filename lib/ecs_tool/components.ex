defmodule EcsTool.Components do
    import EcsTool.Formatter, only: [to_macro: 1, format_macro: 2]

    defstruct [archetype: { [], %{} }, individual: { [], %{} }, names: %{}]

    @type component_type :: :individual | :archetype
    @type name :: String.t
    @type index :: non_neg_integer
    @type t :: %__MODULE__{
        archetype: { [name], %{ index => name } },
        individual: { [name], %{ index => name } },
        names: %{ name => component_type }
    }

    @types %{
        "ECS_COMPONENT" => :individual,
        "ECS_ARCHETYPE_COMPONENT" => :archetype
    }

    def extract(components \\ %__MODULE__{}, string) do
        append(Regex.scan(~r/(#{@types |> Map.keys |> Enum.join("|")})\((.*?)\)/, string, capture: :all_but_first), components)
    end

    defp append([], components), do: components
    defp append([[type, args]|t], components = %{ names: names }) do
        field = @types[type]
        { unordered, ordered } = Map.get(components, field)

        [name|args] = String.split(args, ",")
        name = String.trim(name)

        components = if Map.has_key?(names, name) do
            IO.puts "\"#{name}\" component already exists"
            components
        else
            appended = case args do
                [] -> { [name|unordered], ordered }
                [index] ->
                    { index, _ } = String.trim(index) |> Integer.parse

                    if Map.has_key?(ordered, index) do
                        IO.puts "#{type} already exists for index (#{index}): replacing \"#{ordered[index]}\" with \"#{name}\""
                    end

                    { unordered, Map.put(ordered, index, name) }
            end

            %{ components | field => appended, names: Map.put(names, name, field) }
        end

        append(t, components)
    end

    def kind(components, name), do: components.names[name]

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

    def defines(components, namespace) do
        fun = fn
            nil, { defs, n, type } -> { defs, n + 1, type }
            name, { defs, n, type } -> { [defs, ["#define ", to_macro(name), " ", type, " | ", Integer.to_string(n), "\n"]], n + 1, type }
        end

        Enum.map(@types, fn { _, v } ->
            get(components, v) |> Enum.reduce({ [], 0, "ECSComponentType#{to_string(v) |> String.capitalize}" }, fun) |> elem(0)
        end)
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

    def archetype_deps(components, namespace, relative \\ false, allowed_archs \\ nil) do
        comps = get(components, :archetype)

        count = Enum.count(comps)
        sets = archetype_sets(comps)

        { code, _ } = Enum.reduce(if(allowed_archs, do: Enum.filter(sets, &MapSet.member?(allowed_archs, Enum.sort(&1))), else: sets), { [], { 0, "" } }, fn set, { code, { n, previous } } ->
            pointers = Enum.reduce(sets, [], fn group, acc ->
                indexed_set = Enum.map(set, &Enum.find_index(group, fn x -> x == &1 end))
                if Enum.all?(indexed_set) do
                    size = Enum.count(group) |> to_string
                    [["    ", "{ offsetof(ECSContext, archetypes", size, "[ECS_ARCHETYPE", size, "_INDEX(", Enum.map(group, &to_macro(&1)) |> Enum.join(", "), ")]), ", format_macro("INDEX", namespace), Enum.map(indexed_set, &(["_", to_string(&1)])), " },\n"]|acc]
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
            "ECSArchetypePointer ", namespace, "ArchetypeDependencies[] = {\n",
            code,
            "};\n"
        ]

        counts = Enum.map(1..count, &(["#define ", format_macro("ARCHETYPE", namespace), to_string(&1), "_DEPENDENCIES_COUNT ", to_string(2**(count - &1)), "\n"]))

        access = Enum.map(1..count, &(["#define ", format_macro("COMPONENT_SYSTEM_ACCESS_ARCHETYPE", namespace), to_string(&1), "(", Enum.map(1..&1, fn x -> "x" <> to_string(x) end) |> Enum.join(", "), ")", " { .count = ", format_macro("ARCHETYPE", namespace), to_string(&1), "_DEPENDENCIES_COUNT, .pointer = &", namespace, "ArchetypeDependencies[", format_macro("ARCHETYPE_DEPENDENCIES", namespace), "_##", Enum.map(1..&1, fn x -> "x" <> to_string(x) end) |> Enum.join("##_##"), "] }\n"]))

        { deps, counts, access }
    end

    def component_sizes(components, namespace) do
        Enum.map(@types, fn { _, v } ->
            [
                "const size_t ", namespace, String.capitalize(to_string(v)), "ComponentSizes[] = {\n",
                get(components, v) |> Enum.map(&(["    sizeof(", &1, "),\n"])),
                "};\n"
            ]
        end)
    end

    defp get_indexes(list, a, b), do: { Enum.find_index(list, &match?(^a, &1)), Enum.find_index(list, &match?(^b, &1)) }

    def sort(components, comps) do
        Enum.sort(comps, fn a, b ->
            with { nil, nil } <- get(components, :archetype) |> get_indexes(a, b),
                 { nil, nil } <- get(components, :individual) |> get_indexes(a, b) do
                true
            else
                { nil, _ } -> false
                { _, nil } -> true
                { a_index, b_index } -> a_index < b_index
            end
        end)
    end
end
