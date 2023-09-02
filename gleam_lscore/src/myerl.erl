-module(myerl).
-compile(export_all).

hello()->
    io:format("hello world~n",[]).

untyped(X)->
    X.

dereference(Ref) ->
    erlang:get(Ref).

set_reference(Ref,A) ->
    erlang:put(Ref,A).

make_reference(A) ->
    Ref = erlang:system_time(),
    erlang:put(Ref,A),
    Ref.

delete_reference(Ref) ->
    erlang:erase(Ref).
