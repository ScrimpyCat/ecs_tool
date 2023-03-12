defmodule EcsTool.Formatter do
    def format_macro(name, ""), do: name
    def format_macro(name, namespace), do: [namespace, "_", name]

    def to_macro(name, namespace \\ ""), do: name |> String.replace(~r/([0-9]*[A-Z]+[0-9]*)+/, "_\\0") |> String.replace(~r/_(([0-9]*[A-Z]+[0-9]*)+)([A-Z0-9])([a-z_])/, "_\\2_\\3\\4") |>  String.trim_leading("_") |> String.upcase |> format_macro(namespace)
end
