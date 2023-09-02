-module(ffi_gleam_pp).
-compile(export_all).
-import(io_lib,[format/2]).

typeof(T) ->
    case T of
        t_unknown -> "";
        _ -> format("~p::",[T])
    end.

pp({expr2,Expr,_}) ->
    pp(Expr);

pp({block,T,Lst}) ->
    LS = lists:flatten( lists:map(fun(E)-> [pp(E),"\n"] end,Lst) ),
    format("~s{\n~s}",[typeof(T),LS]);
pp({sequence,T,Lst}) ->
    LS = lists:flatten( lists:map(fun(E)-> [pp(E)," "] end,Lst) ),
    format("~s(~s)",[typeof(T),LS]);

pp({'list_ex',T,Lst}) ->
    [_|Lst2] = lists:flatten( lists:map(fun(E)-> [",",pp(E)] end,Lst) ),
    %io:format("~p\n",[Lst2]),
    %SLst = lists:concat( Lst2 ),
    SLst = Lst2,
    format("~s[~s]",[typeof(T),SLst]);

pp({'let',T,A,B}) ->
    SA = pp(A),
    SB = pp(B),
    format("~slet ~s: ~p = ~s",[typeof(T),SA,T,SB]);

pp({def_arg,T,Name,Renamed,Default}) ->
    case Default of
        none -> format("~s",[Name]);
        Val -> format("~s = ~s",[Name,pp(Default)])
    end;

pp({def_param,T,Name,Expr}) ->
    case Name of
        none -> format("~s",[pp(Expr)]);
        Val -> format("~s: ~s",[Name,pp(Expr)])
    end;

pp({'fun',T,Name,Args,Expr}) ->
    [_|Args2] = lists:flatten( lists:map(fun(E)-> [",",pp(E)] end,Args) ),
    %SArgs = lists:concat( Args2 ),
    SArgs = Args2,
    SExpr = pp(Expr),
    format("fn ~s(~s) -> ~p\n~s",[Name,SArgs,T,SExpr]);

pp({name,T,Name}) ->
    format("~s~s",[typeof(T),Name]);

pp({renamed,T,Name,Rename}) ->
    format("~s~s",[typeof(T),Name]);

pp({funcall,T,Fn,[]}) ->
    SFn = pp(Fn),
    format("~s~s()",[typeof(T),SFn]);

pp({funcall,T,Fn,Params}) ->
    SFn = pp(Fn),
    [_|Params2] = lists:flatten( lists:map(fun(E)-> [",",pp(E)] end,Params) ),
    %SParams = lists:concat( Params2 ),
    SParams = Params2,
    format("~s~s(~s)",[typeof(T),SFn,SParams]);

pp({string_val,Val}) ->
    Val;

pp({int_val,Val}) ->
    format("~p",[Val]);

pp({bin_op,T,Name,A,B}) ->
    SA = pp(A),
    SB = pp(B),
    case Name of
        plus -> format("~s + ~s",[SA,SB]);
        minus -> format("~s - ~s",[SA,SB]);
        star -> format("~s * ~s",[SA,SB]);
        slash -> format("~s / ~s",[SA,SB])
    end;

pp({member_access,T,Expr,Member}) ->
    format("~s.~s",[pp(Expr),Member]);

pp(X) ->
    format("~p",[X]).

