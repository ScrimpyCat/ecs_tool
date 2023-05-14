defmodule EcsTool.Config do
    import Bitwise

    def defines(n) do
        comp_max = [
            "#ifndef ECS_ARCHETYPE_COMPONENT_MAX\n",
            "#define ECS_ARCHETYPE_COMPONENT_MAX ", to_string(n), "\n",
            "#endif\n"
        ]

        encoders = Enum.reduce(n..0, [], fn i, encodes ->
            ["#define ECS_ARCHETYPE_ENCODE_", to_string(i), "(x) ", arch_encode(i), "\n"|encodes]
        end)

        combiner = "#define ECS_ARCHETYPE_ENCODE_COMBINE(a, b) (a) + (b)\n"

        indexes = arch_indexes(n)

        maxes = Enum.reduce(n..1, [], fn i, acc ->
            [[
                "#ifndef ECS_COMPONENT_ARCHETYPE", to_string(i), "_MAX\n",
                "#define ECS_COMPONENT_ARCHETYPE", to_string(i), "_MAX ECS_ARCHETYPE_ENCODE_", to_string(i - 1), "(ECS_ARCHETYPE_COMPONENT_MAX)\n",
                "#endif\n\n"
            ]|acc]
        end)

        max = [
            "#ifndef ECS_ARCHETYPE_MAX\n",
            "#define ECS_ARCHETYPE_MAX ECS_ARCHETYPE_COMPONENT_MAX\n",
            "#elif ECS_ARCHETYPE_MAX > ", to_string(n), "\n",
            "#error ECS_ARCHETYPE_MAX exceeds maximum limit. Regenerate config to support required size.\n",
            "#endif\n"
        ]

        packed_comp_max = [
            "#ifndef ECS_PACKED_COMPONENT_MAX\n",
            "#define ECS_PACKED_COMPONENT_MAX 64\n",
            "#endif\n"
        ]

        indexed_comp_max = [
            "#ifndef ECS_INDEXED_COMPONENT_MAX\n",
            "#define ECS_INDEXED_COMPONENT_MAX 64\n",
            "#endif\n"
        ]

        dup_comp_max = [
            "#ifndef ECS_DUPLICATE_COMPONENT_MAX\n",
            "#define ECS_DUPLICATE_COMPONENT_MAX 64\n",
            "#endif\n"
        ]

        comp_index = "#define ECS_COMPONENT_INDEX(x) ((x) & ~ECSComponentTypeMask)\n"

        all_comp_max = "#define ECS_COMPONENT_MAX (ECS_ARCHETYPE_COMPONENT_MAX + ECS_PACKED_COMPONENT_MAX + ECS_DUPLICATE_COMPONENT_MAX)\n"

        [
            comp_max, "\n",
            encoders, "\n",
            combiner, "\n",
            indexes, "\n",
            maxes,
            max, "\n",
            packed_comp_max, "\n",
            indexed_comp_max, "\n",
            dup_comp_max, "\n",
            all_comp_max, "\n",
            comp_index
        ]
    end

    defp arch_encode(n), do: ["(", arch_encode_axes(1, n, ["(x)"]), " / ", to_string(n + 1), ")"]

    defp arch_encode_axes(i, n, iodata) when i <= n, do: arch_encode_axes(i + 1, n, ["((x) - ", to_string(i), ") / ", to_string((n - i) + 1), " * "|iodata])
    defp arch_encode_axes(_, _, iodata), do: iodata

    defp arch_indexes(n) do
        Enum.drop(n..0, 1)
        |> Enum.map(&([["x", to_string(&1)], ", "]))
        |> arch_indexes(n, [])
    end

    defp arch_indexes([[arg1|_], [arg2|_]|t], n, indexes) do
        args = Enum.reverse([arg2|t])
        arch_indexes([[arg2, ""]|t], n - 1, [define_arch_index(n, [args, ", ", arg1], arg1, ["(", args, ")"])|indexes])
    end
    defp arch_indexes([], _, indexes), do: ["#define ECS_ARCHETYPE0_INDEX 0\n"|indexes]
    defp arch_indexes([[arg|_]], n, indexes) do
        arch_indexes([], n - 1, [define_arch_index(n, [arg], arg, [])|indexes])
    end

    defp define_arch_index(n, args, encode_arg, next_args) do
        next = n - 1
        ["#define ECS_ARCHETYPE", to_string(n), "_INDEX(", args, ") ECS_ARCHETYPE_ENCODE_COMBINE(ECS_ARCHETYPE_ENCODE_", to_string(next), "(", encode_arg, "), ECS_ARCHETYPE", to_string(next), "_INDEX", next_args, ")\n"]
    end

    defp index_args(0, _, args), do: args
    defp index_args(i, n, args) do
        index_args(i - 1, n, ["ECS_ARCHETYPE_COMPONENT_MAX - ", to_string(n - i), ", "|args])
    end
end
