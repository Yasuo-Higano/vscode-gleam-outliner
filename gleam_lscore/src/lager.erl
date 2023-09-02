-module(lager).
-compile(export_all).

notice(Fmt,Param) ->
    io:format(Fmt,Param),io:format("\n",[]).

info(Fmt,Param) ->
    notice(Fmt,Param).

error(Fmt,Param) ->
    notice(Fmt,Param).
