-module(ffi_erlang_json).
-compile(export_all).

get_nt_p(Json,Key) ->
    {PList} = Json,
    Value = proplists:get_value(Key,PList).