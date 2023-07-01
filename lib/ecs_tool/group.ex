defmodule EcsTool.Group do
    import EcsTool.Formatter, only: [to_macro: 2, format_macro: 2]
    import Bitwise

    defstruct [name: nil, frequency: "0", dynamic: "false", priorities: []]

    @type name :: String.t

    defmodule Priority do
        defstruct [systems: [], dependency: nil]

        @type t :: % __MODULE__{
            systems: [EcsTool.Group.System.name],
            dependency: { EcsTool.Group.name, String.t } | nil
        }
    end

    @type t :: %__MODULE__{
        name: name | nil,
        frequency: String.t,
        dynamic: String.t,
        priorities: [Priority.t | nil]
    }
    @type groups :: %{ name => t }

    def extract(groups \\ %{}, string) do
        append(Regex.scan(~r/ECS_SYSTEM_GROUP\((.+?),(.+?),(.+?)(,\s*PRIORITY\(.+?,\s*\(.*?\)\s*(,\s*{.*?}\s*)?\)\s*)*,?\s*\)/, string), groups)
    end

    defp append([], groups), do: groups
    defp append([[match, name, freq, dynamic|_]|t], groups) do
        name = String.trim(name)

        groups = if Map.has_key?(groups, name) do
            IO.puts "\"#{name}\" group already exists"
            groups
        else
            { _, priorities } = Regex.scan(~r/PRIORITY\((.*?),.*?\((.*?)\).*?(,.*?\{(.*?)\})?.*?\)/, match, capture: :all_but_first) |> Enum.map(fn [priority, systems|t] ->
                    { priority, _ } = priority |> String.trim_leading |> Integer.parse
                    systems = systems |> String.split(",") |> Enum.map(&String.trim/1)
                    dep = case t do
                        [_, dep] -> dep |> String.split(",") |> Enum.map(&String.trim/1) |> List.to_tuple
                        _ -> { "-1", "-1" }
                    end
                    { priority, systems, dep }
            end) |> Enum.sort(fn { a, _, _ }, { b, _, _ } ->
                a < b
            end) |> Enum.reduce({ 0, [] }, fn { priority, systems, dep }, { n, acc } ->
                [_|acc] = Enum.reduce(0..(priority - n), acc, fn _, acc ->
                    [%EcsTool.Group.Priority{ systems: [], dependency: { "-1", "-1" } }|acc]
                end)

                { priority + 1, [%EcsTool.Group.Priority{ systems: systems, dependency: dep}|acc] }
            end)

            Map.put(groups, name, %EcsTool.Group{
                name: name,
                frequency: String.trim(freq),
                dynamic: String.trim(dynamic),
                priorities: Enum.reverse(priorities)
            })
        end

        append(t, groups)
    end

    def defines(groups) do
        { _, defines } =
            groups
            |> Map.keys
            |> Enum.reduce({ 0, [] }, fn name, { n, defines } ->
                { n + 1, [["#define ", name, " ", to_string(n), "\n"]|defines] }
            end)

        defines
    end

    def dependencies(groups, namespace) do
        { code, _ } = Enum.reduce(groups, { [], 0 }, fn { k, %{ priorities: priorities } }, { code, n } ->
            code = [code, "#define ", [k, "_DEPENDENCIES"], " (", namespace, "GroupDependencies + ", to_string(n), ")\n"]

            initialisers = Enum.map(priorities, fn
                %{ dependency: nil } -> []
                %{ dependency: { group, priority } } -> ["    { .group = ", group, ", .priority = ", priority, " },\n"]
            end)

            { [code, initialisers], n + Enum.count(initialisers) }
        end)

        [
            "const ECSGroupDependency ", namespace, "GroupDependencies[] = {\n",
            code,
            "};\n"
        ]
    end

    def system_range(groups, namespace) do
        { code, _ } = Enum.reduce(groups, { [], 0 }, fn { k, %{ priorities: priorities } }, { code, n } ->
            code = [code, "#define ", [k, "_SYSTEM_RANGE"], " (", namespace, "SystemRange + ", to_string(n), ")\n"]

            { initialisers, _ } = Enum.reduce(priorities, { [], 0 }, fn
                %{ systems: systems }, { acc, last } ->
                    count = Enum.count(systems)
                    { [acc, ["    { ", to_string(last), ", ", to_string(count), " },\n"]], last + count }
            end)

            { [code, initialisers], n + Enum.count(priorities) }
        end)

        [
            "const ECSSystemRange ", namespace, "SystemRange[] = {\n",
            code,
            "};\n"
        ]
    end

    def system_update(groups, systems, namespace) do
        { code, _ } = Enum.reduce(groups, { [], 0 }, fn { _, %{ name: name, priorities: priorities } }, { code, n } ->
            { updates, count } = Enum.reduce(priorities, { [], 0 }, fn %{ systems: names }, { acc, count } ->
                values = Enum.map(names, fn name ->
                    case systems[name] do
                        %{ parallel: false } -> ["    ECS_SYSTEM_UPDATE(", name, "),\n"]
                        %{ parallel: true } -> ["    ECS_SYSTEM_UPDATE_PARALLEL(", name, "),\n"]
                    end
                end)

                { [acc, values], count + Enum.count(values) }
            end)

            {
                [
                    code,
                    "#define ", name, "_SYSTEM_UPDATE (", namespace, "SystemUpdate + ", to_string(n), ")\n",
                    updates
                ],
                n + count
            }
        end)

        [
            "const ECSSystemUpdate ", namespace, "SystemUpdate[] = {\n",
            code,
            "};\n"
        ]
    end

    def system_access(groups, systems, components, namespace) do
        { code, _ } = Enum.reduce(groups, { [], 0 }, fn { _, %{ name: name, priorities: priorities } }, { code, n } ->
            { accesses, count } = Enum.reduce(priorities, { [], 0 }, fn %{ systems: names }, { acc, count } ->
                values = Enum.map(names, fn name ->
                    %{ read: read, write: write } = systems[name]

                    { arch_set, comp_set } = (read ++ write) |> Enum.reduce({ [], [] }, fn comp, { arch_acc, comp_acc } ->
                        case EcsTool.Components.kind(components, comp) do
                            :archetype -> { [comp|arch_acc], comp_acc }
                            _ -> { arch_acc, [comp|comp_acc] }
                        end
                    end)

                    arch_deps = case arch_set do
                        [] -> []
                        set -> [", .archetype = COMPONENT_SYSTEM_ACCESS_ARCHETYPE", to_string(Enum.count(set)), "(", set |> Enum.sort |> Enum.join(", "), ")"]
                    end
                    comp_field = case comp_set do
                        [] -> []
                        set -> [", .component = { .offsets = ", concat_macro(set, format_macro("COMPONENT_OFFSET_LIST", namespace)), " }"]
                    end

                    ["    { .read = ", format_access_list(read, namespace), ", .write = ", format_access_list(write, namespace), arch_deps, comp_field, " },\n"]
                end)

                { [acc, values], count + Enum.count(values) }
            end)

            {
                [
                    code,
                    "#define ", name, "_SYSTEM_ACCESS (", namespace, "SystemAccess + ", to_string(n), ")\n",
                    accesses
                ],
                n + count
            }
        end)

        [
            "const ECSSystemAccess ", namespace, "SystemAccess[] = {\n",
            code,
            "};\n"
        ]
    end

    defp concat_macro(names, macro), do: [macro, Enum.sort(names) |> Enum.map(&(["_", &1]))]

    defp format_access_list([], _), do: "{ .ids = NULL, .count = 0 }"
    defp format_access_list(names, namespace), do: ["{ .ids = ", concat_macro(names, format_macro("COMPONENT_ID_LIST", namespace)), ", .count = ", to_string(Enum.count(names)), " }"]

    defp system_access_masks(names, systems) do
        { values, set } = Enum.reduce(names, { [], MapSet.new }, fn name, { acc, set } ->
            %{ read: read, write: write } = systems[name]
            read = MapSet.new(read)
            write = MapSet.new(write)
            { [{ name, read, write }|acc], set |> MapSet.union(read) |> MapSet.union(  write) }
        end)

        ordered_set = MapSet.to_list(set) |> Enum.sort
        max = Enum.count(ordered_set)

        values = Enum.reduce(values, [], fn { name, read, write }, acc ->
            { index, _ } = Enum.reduce(ordered_set, { 0, 0 }, fn comp, { sum, n } ->
                mask = cond do
                    MapSet.member?(write, comp) -> 1 <<< max
                    MapSet.member?(read, comp) -> 1
                    true -> 0
                end

                { sum ||| (mask <<< n), n + 1 }
            end)

            [{ name, read, write, index }|acc]
        end)

        { values, max }
    end

    defp sort_access_masks(masks) do
        Enum.sort(masks, fn { _, _, _, a }, { _, _, _, b } ->
            a <= b
        end)
    end

    def sort_systems(groups, systems) do
        Enum.reduce(groups, %{}, fn { key, group = %{ priorities: priorities } }, sorted_groups ->
            priorities = Enum.map(priorities, fn priority = %{ systems: names } ->
                { masks, _ } = system_access_masks(names, systems)
                %{ priority | systems: sort_access_masks(masks) |> Enum.map(fn { name, _, _, _ } -> name end) }
            end)

            Map.put(sorted_groups, key, %{ group | priorities: priorities })
        end)
    end

    defp graph(masks, max), do: graph(masks, masks, max)

    defp graph([mask|l], masks, max), do: <<graph(mask, masks, max, <<>>) :: bitstring, graph(l, masks, max) :: bitstring>>
    defp graph([], _, _), do: <<>>

    defp graph(mask = { name, _, _, _ }, [{ name, _, _, _ }|l], max, bits), do: graph(mask, l, max, <<bits :: bitstring, 0 :: 1>>)
    defp graph(mask = { _, _, _, a }, [{ _, _, _, b }|l], max, bits) do
        m = Itsy.Bit.set(max)

        a_w = a >>> max
        b_w = b >>> max
        a_r = a &&& m
        b_r = b &&& m

        conn = if (bxor(a_w, b_r) == (a_w ||| b_r)) and (bxor(a_r, b_w) == (a_r ||| b_w)) and (bxor(a_w, b_w) == (a_w ||| b_w)) do
            1
        else
            0
        end

        graph(mask, l, max, <<bits :: bitstring, conn :: 1>>)
    end
    defp graph(_, [], _, bits) do
        n = bit_size(bits)
        pad = rem(8 - (n - (8 * div(n, 8))), 8)
        <<bits :: bitstring, 0 :: size(pad)>>
    end

    defp flip(byte) do
        byte = (byte &&& 0xf0) >>> 4 ||| (byte &&& 0x0f) <<< 4
        byte = (byte &&& 0xcc) >>> 2 ||| (byte &&& 0x33) <<< 2
        byte = (byte &&& 0xaa) >>> 1 ||| (byte &&& 0x55) <<< 1
    end

    def system_graph(groups, systems, namespace) do
        { code, _ } = Enum.reduce(groups, { [], 0 }, fn { _, %{ name: name, priorities: priorities } }, { code, n } ->
            { graphs, count } = Enum.reduce(priorities, { [], 0 }, fn %{ systems: names }, { acc, count } ->
                { masks, max } = system_access_masks(names, systems)

                bits = <<graph(masks, max) :: bitstring, 0 :: 7>>
                bytes = for <<byte :: 8 <- bits>>, do: [Integer.to_string(flip(byte)), ", "]

                { [acc, "    ", bytes, "\n"] , count + Enum.count(bytes) }
            end)

            {
                [
                    code,
                    "#define ", name, "_SYSTEM_GRAPH (", namespace, "SystemGraph + ", to_string(n), ")\n",
                    graphs
                ],
                n + count
            }
        end)

        [
            "const uint8_t ", namespace, "SystemGraph[] = {\n",
            code,
            "};\n"
        ]
    end

    def groups(groups, namespace) do
        code = Enum.map(groups, fn { name, %{ frequency: freq, dynamic: dynamic, priorities: priorities } } ->
            [
                "    {\n",
                "        .freq = ", freq, ",\n",
                "        .dynamic = ", dynamic, ",\n",
                "        .priorities = {\n",
                "            .count = ", to_string(Enum.count(priorities)), ",\n",
                "            .deps = ", name, "_DEPENDENCIES,\n",
                "            .systems = {\n",
                "                .range = ", name, "_SYSTEM_RANGE,\n",
                "                .graphs = ", name, "_SYSTEM_GRAPH,\n",
                "                .update = ", name, "_SYSTEM_UPDATE,\n",
                "                .access = ", name, "_SYSTEM_ACCESS,\n",
                "            }\n",
                "        }\n",
                "    },\n"
            ]
        end)

        [
            "const ECSGroup ", namespace, "Groups[] = {\n",
            code,
            "};\n"
        ]
    end
end
