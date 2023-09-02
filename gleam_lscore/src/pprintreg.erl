-module(pprintreg).

-compile(export_all).

start() ->
    io:format("~p started.\n",[?MODULE]),
    ets:new(?MODULE,[set,named_table,public]).

get_ppfn(Name)->
    case ets:lookup(?MODULE,Name) of
        [{_,Fn}] ->
            {ok,Fn};

        _ ->
            {error,<<"no_entry">>}
    end.

apply_ppfn(DynamicValue) ->
    TypeName = element(1, DynamicValue),
    {ok, Fn} = get_ppfn(TypeName),
    Fn(DynamicValue).


% --------------------------------------------------

register_ppfn(Name,Fn) ->
    TypeName = case is_tuple(Name) of
        true ->
            element(1,Name);
        _ ->
            Name
        end,

    ets:insert(?MODULE,[{TypeName,Fn}]).

