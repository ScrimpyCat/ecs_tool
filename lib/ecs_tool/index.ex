defmodule EcsTool.Index do
    import EcsTool.Formatter, only: [format_macro: 2]

    defmodule Set do
        def encode(set, n \\ 1, factorial \\ 1)
        def encode([], _, _), do: 0
        def encode([x|set], n, factorial), do: div(product(x, n - 1), (factorial * n)) + encode(set, n + 1, factorial * n)

        defp product(x, 0), do: x
        defp product(x, n), do: (x - n) * product(x, n - 1)
    end

    defp wrap(x, max) when x + 2 >= max, do: max
    defp wrap(x, _), do: x + 2

    defp next([max], max), do: [max]
    defp next([max|set], max), do: [wrap(hd(set), max)|next(set, max - 1)]
    defp next([x|set], _), do: [x + 1|set]

    defp set(max), do: (max..1) |> Enum.to_list

    def series(max) do
        series(set(max), max, max)
    end

    defp series(_, 0, _), do: []
    defp series([], _, _), do: []
    defp series(set, n, max) do
        next_set  = next(set, max)
        sets = if next_set == set do
            series(set(n-1), n-1, max)
        else
            series(next_set, n, max)
        end

        [[0|Enum.reverse(set)]|sets]
    end

    defp seqs(set), do: seqs(set, Enum.count(set))

    defp seqs(_, 0), do: []
    defp seqs(set, n), do: [seqs(set, n, 0)|seqs(set, n-1)]

    defp seqs(set, n, i) when length(set) >= n do
        seq = Enum.take(set, n)
        [{ seq, i }|seqs(tl(set), n, i + 1)]
    end
    defp seqs(_, _, _), do: []

    defp filter(seqs, indexes, allowed \\ nil, list \\ [])
    defp filter([], _, _, list), do: list
    defp filter([h|t], indexes, allowed, list) do
        filter(t, indexes, allowed, Enum.reduce(h, list, fn define = { index, _ }, acc ->
            if Map.has_key?(indexes, index) do
                acc
            else
                case allowed do
                    nil -> [define|acc]
                    %{ ^index => _ } -> [define|acc]
                    _ -> acc
                end
            end
        end))
    end

    def group(sets, namespace, allowed \\ nil, groups \\ [], indexes \\ %{}, offset \\ 0)
    def group([], namespace, _, groups, indexes, _) do
        defines = Enum.map(indexes, fn { k, v } ->
            [
                "\#define ", format_macro("INDEX", namespace),
                k |> Enum.map(&(["_", to_string(&1)])),
                " ",
                to_string(v),
                ", ",
                k |> Enum.count |> to_string,
                "\n"
            ]
        end)
        { ["const size_t ", namespace, "ArchetypeComponentIndexes[] = {\n", groups, "};\n"], defines }
    end
    def group([set|sets], namespace, allowed, groups, indexes, offset) do
        seqs(set)
        |> filter(indexes, allowed)
        |> case do
            [] -> group(sets, namespace, allowed, groups, indexes, offset)
            seqs ->
                indexes = Enum.reduce(seqs, indexes, fn { k, v }, acc ->
                    Map.put(acc, k, v + offset)
                end)

                group(sets, namespace, allowed, [groups, "    ", set |> Enum.map(&Integer.to_string/1) |> Enum.join(", "), ",\n"], indexes, offset + Enum.count(set))
        end
    end
end
