-module(ffi_erlang_str).
-compile(export_all).

to_gleam_str(Str) ->
    if is_list(Str) ->
            list_to_binary(Str);
       is_binary(Str) ->
            Str
    end.