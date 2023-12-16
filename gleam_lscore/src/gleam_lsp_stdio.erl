-module(gleam_lsp_stdio).
-compile(export_all).

append_to_file(File, Fmt,Prm) ->
    Text = lists:flatten( io_lib:format(Fmt,Prm) ),
    case file:open(File, [append]) of
        {ok, IoDevice} ->
            io:format(IoDevice, "~s~n", [Text]),
            file:close(IoDevice);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

dbg_w(Fmt,Prm)->
    append_to_file(standard_error,Fmt,Prm).

log_w(Fmt,Prm)->
    append_to_file(standard_error,Fmt,Prm).

start()->
    io:setopts(standard_io,[binary,{encoding,latin1}]),
    LSP = gleam_lsp@languageserver@server:start(),
    loop(LSP).

read_line()->
    case file:read(standard_io,1) of
        {ok, <<X>>} -> if X =< 32 -> read_line();
                          true -> read_line_(<<X>>)
                       end;

        A -> log_w("~p\n",[A])
    end.

read_line_(Acc)->
    case file:read(standard_io,1) of
        {ok, <<13>>} -> {ok, Acc};
        {ok, <<10>>} -> read_line_(Acc);
        {ok, Data} ->
             read_line_(<<Acc/binary,Data/binary>>);
        A -> log_w("~p\n",[A])
    end.
    
skip(N) ->
    {ok,Skip} = file:read(standard_io,N),
    %dbg_w("skip> ~p\n",[Skip]),
    Skip.

loop(LSP) ->
    %dbg_w("read...\n",[]),
    Header = read_line(),
    %dbg_w("header: ~p\n",[Header]),
    {ok,<<"Content-Length: "/utf8,SContentLen/binary>>} = Header,
    ConvInt = string:to_integer(SContentLen),
    {ContentLen,_} = string:to_integer(SContentLen),
    %dbg_w("content-length: ~p\n",[ContentLen]),
    skip(3),
    Content = case file:read(standard_io,ContentLen) of
        {ok,Bin} ->
            %dbg_w("read: ~p / ~p\n",[byte_size(Bin),ContentLen]),
            Bin;
        Err ->
            log_w("read error: ~p\n",[Err]),
            <<>>
        end,
    proc(LSP,Content),
    loop(LSP).

proc(LSP,Content)->
    %dbg_w("|~s\n",[Content]),
    Res = gleam_lsp@languageserver@server:handle_message(LSP,Content),
    case Res of
        {some,Msg} ->
            %dbg_w("< Content-Length: ~p\r\n\r\n",[byte_size(Msg)]),
            %dbg_w("< ~ts\n",[Msg]),
            io:format(standard_io,"Content-Length: ~p\r\n\r\n~ts",[byte_size(Msg),Msg]),
            %io:format(standard_io,"Content-Length: ~p\r\n\r\n",[byte_size(Msg)]),
            %file:write(standard_io, Msg),
            %io:flush();
            io:put_chars(<<>>);

        none ->
            log_w("no_reply\n",[]),
            [];
        Unknown -> log_w("?? ~p\n",[Unknown])
    end.

