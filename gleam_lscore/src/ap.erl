-module(ap).
-compile(export_all).

start()->
    erlang_runtime:load_all_modules([]),
    erlang_runtime:init_all().

run(Module) ->
    start(),
    Module:run().
