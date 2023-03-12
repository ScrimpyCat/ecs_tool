defmodule EcsTool.Components do
    import EcsTool.Formatter, only: [to_macro: 1, format_macro: 2]

    defstruct [archetype: { [], %{} }, individual: { [], %{} }, names: MapSet.new]

    @type name :: String.t
    @type index :: non_neg_integer
    @type t :: %__MODULE__{
        archetype: { [name], %{ index => name } },
        individual: { [name], %{ index => name } },
        names: MapSet.t(name)
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

        components = if MapSet.member?(names, name) do
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

            %{ components | field => appended, names: MapSet.put(names, name) }
        end

        append(t, components)
    end

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

    def gen_sets(list), do: gen_sets(list, list, Enum.count(list), [])

    defp gen_sets(_, _, 0, append), do: append
    defp gen_sets(_, [], _, _), do: []
    defp gen_sets(list, [h|l], n, append) do
        [gen_sets(list, list, n - 1, [h|append])|gen_sets(list, l, n, append)]
    end

    defp flatten([], list), do: list
    defp flatten([h|t], list) when is_list(h), do: flatten(t, flatten(h, list))
    defp flatten(x, list), do: [x|list]

    defp to_arch_indexes(set, comps), do: Enum.map(set, fn x -> Enum.find_index(comps, &match?(^x, &1)) end)

    def archetype_sets(components) do
        comps = get(components, :archetype)

        count = Enum.count(comps)
        nil_comps = [nil|comps]

        gen_sets(nil_comps, nil_comps, count, [])
        |> flatten([])
        |> Enum.reduce(MapSet.new, fn set, acc ->
            set
            |> Enum.filter(&(!is_nil(&1)))
            |> Enum.uniq
            |> Enum.sort
            |> case do
                [] -> acc
                s -> MapSet.put(acc, s)
            end
        end)
        |> MapSet.to_list
        |> Enum.sort(&({ Enum.count(&1), to_arch_indexes(&1, comps) } > { Enum.count(&2), to_arch_indexes(&2, comps) }))
    end

    def archetype_deps(components, namespace, relative \\ false, allowed_archs \\ nil) do
        comps = get(components, :archetype)

        count = Enum.count(comps)
        nil_comps = [nil|comps]

        sets =
            gen_sets(nil_comps, nil_comps, count, [])
            |> flatten([])
            |> Enum.reduce(MapSet.new, fn set, acc ->
                set
                |> Enum.filter(&(!is_nil(&1)))
                |> Enum.uniq
                |> Enum.sort
                |> case do
                    [] -> acc
                    s -> MapSet.put(acc, s)
                end
            end)
            |> MapSet.to_list
            |> Enum.sort(&({ Enum.count(&1), to_arch_indexes(&1, comps) } > { Enum.count(&2), to_arch_indexes(&2, comps) }))

        { code, _ } = Enum.reduce(if(allowed_archs, do: Enum.filter(sets, &MapSet.member?(allowed_archs, &1)), else: sets), { [], { 0, "" } }, fn set, { code, { n, previous } } ->
            pointers = Enum.reduce(sets, [], fn group, acc ->
                indexed_set = Enum.map(set, &Enum.find_index(group, fn x -> x == &1 end))
                if Enum.all?(indexed_set) do
                    size = Enum.count(group) |> to_string
                    [["    ", "{ offsetof(ECSWorld, archetypes", size, "[ECS_ARCHETYPE", size, "_INDEX(", Enum.map(group, &to_macro(&1)) |> Enum.join(", "), ")]), ", format_macro("INDEX", namespace), Enum.map(indexed_set, &(["_", to_string(&1)])), " },\n"]|acc]
                else
                    acc
                end
            end)

            current = [format_macro("ARCHETYPE_DEPENDENCIES", namespace), Enum.map(set, &(["_", &1]))]

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
end
