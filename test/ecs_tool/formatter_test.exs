defmodule EcsTool.FormatterTest do
    use ExUnit.Case
    doctest EcsTool.Formatter

    test "to_macro formatting" do
        assert "FOO" == EcsTool.Formatter.to_macro("foo")
        assert "FOO" == EcsTool.Formatter.to_macro("Foo")
        assert "FOO_A" == EcsTool.Formatter.to_macro("FooA")
        assert "FOO_BAR" == EcsTool.Formatter.to_macro("FooBar")
        assert "INT8_T" == EcsTool.Formatter.to_macro("int8_t")
        assert "INT64_T" == EcsTool.Formatter.to_macro("int64_t")
        assert "CC_VECTOR_2D" == EcsTool.Formatter.to_macro("CCVector2D")
        assert "FOO_API_BAR" == EcsTool.Formatter.to_macro("FooAPIBar")
    end
end
