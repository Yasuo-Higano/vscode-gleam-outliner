-module(type_conv).
-compile(export_all).

to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_atom(X) -> atom_to_binary(X).

to_atom(X) when is_binary(X) -> binary_to_atom(X);
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_atom(X) -> X.

to_lua(X) when is_tuple(X) ->
    tuple_to_list(X);
to_lua(X) -> X.