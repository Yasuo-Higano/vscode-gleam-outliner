-module(gleam_lsp_sock).
-compile(export_all).

append_to_file(File, Fmt,Prm) ->
    Text = lists:flatten( io_lib:format(Fmt,Prm) ),
    io:format("# gleam outliner: ~s\n",[Text]),
    case file:open(File, [append]) of
        {ok, IoDevice} ->
            io:format(IoDevice, "~s~n", [Text]),
            file:close(IoDevice);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

dbgout(IoStr) ->
    %append_to_file("./debug.log", lists:flatten( IoStr ) ).
    %io:write( lists:flatten( IoStr ) ).
    io:format( "~ts\n",[lists:flatten( IoStr )] ).

dbg_w(Fmt,Prm)->
    dbgout( io_lib:format(Fmt,Prm) ).

to_str(X) when is_list(X) ->
    lists:flatten( lists:map( fun(X) -> to_str_(X) end, X ) );

to_str(X) ->
    to_str_(X).

to_str_({ok,Msg}) -> io_lib:format("Ok(~ts)",[to_str(Msg)]);
to_str_(Msg) when is_binary(Msg) -> Msg;
to_str_(Msg) when is_list(Msg) -> Msg;
to_str_(Msg) when is_atom(Msg) -> atom_to_list(Msg);
to_str_(Msg) -> io_lib:format("~p",[Msg]).



dbg_1(Msg) ->
    dbg_w( "~ts\n",[to_str(Msg)]).

dbg_2(Msg1,Msg2) ->
    dbg_w( "~ts, ~ts\n",[to_str(Msg1),to_str(Msg2)]).

dbg_3(Msg1,Msg2,Msg3) ->
    dbg_w( "~ts, ~ts, ~ts\n",[to_str(Msg1),to_str(Msg2),to_str(Msg3)]).

log_w(Fmt,Prm)->
    dbg_w(Fmt,Prm).

read_line(Socket)->
    case gen_tcp:recv(Socket,1) of
        {ok, <<X>>} -> if %X =< 32 -> read_line(Socket);
                          true -> read_line_(Socket,<<X>>)
                       end;

        A -> log_w("?????1 ~p\n",[A])
    end.

read_line_(Socket,Acc)->
    case gen_tcp:recv(Socket,1) of
        {ok, <<13>>} -> {ok, Acc};
        {ok, <<10>>} -> read_line_(Socket,Acc);
        {ok, Data} ->
             read_line_(Socket,<<Acc/binary,Data/binary>>);
        A -> log_w("????2 ~p\n",[A])
    end.
    
skip(Socket,N) ->
    {ok,Skip} = gen_tcp:recv(Socket,N),
    Skip.

read_packet({LSP,Socket,{Read,ReadLine,Write}}=Env) ->
    %dbg_1( color:green("read...") ),
    Header = ReadLine(),
    %dbg_2( color:green("header:"),color:cyan(Header) ),
    {ok,<<"Content-Length: "/utf8,SContentLen/binary>>} = Header,
    ConvInt = string:to_integer(SContentLen),
    {ContentLen,_} = string:to_integer(SContentLen),
    %dbg_2( color:green("content-length"),color:cyan(ContentLen) ),
    skip(Socket,3),
    Content = case Read(ContentLen) of
        {ok,Bin} ->
            %dbg_3( color:green("read"), color:cyan(byte_size(Bin)),color:white(ContentLen)),
            Bin;
        Err ->
            %dbg_2(color:red("read error"),color:magenta(Err) ),
            <<>>
    end,
    proc(Env,Content).

proc({LSP,Socket,{Read,ReadLine,Write}}=Env,Content)->
    %dbg_w("|~s\n",[Content]),
    Res = gleam_lsp@languageserver@server:handle_message(LSP,Content),
    case Res of
        {some,Msg} ->
            %dbg_w("< Content-Length: ~p\r\n\r\n",[byte_size(Msg)]),
            %dbg_w("< ~ts\n",[Msg]),
            Text = lists:flatten( io_lib:format("Content-Length: ~p\r\n\r\n~ts",[byte_size(Msg),Msg]) ),
            Write( list_to_binary(Text) );

        none ->
            %log_w("no_reply\n">>,[]),
            [];
        Unknown -> log_w("?? ~p\n",[Unknown])
    end.

% socket
% packet
%   - 4bytes length
%   - length bytes data
socket_receive_loop({LSP,Socket,IoInterface} = State) ->
    read_packet(State),
    socket_receive_loop(State).

socket_accept_loop(ListenSocket=Env) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("# gleam outliner: accepted\n", []),
            Read = fun(N) ->
                gen_tcp:recv(Socket,N)
            end,
            ReadLine = fun() ->
                read_line(Socket)
            end,

            Write = fun(Line) ->
                %io:format("socket_write: ~p\n", [Line]),
                gen_tcp:send(Socket,Line)
            end,

            io:format("# gleam outliner: starting language server\n", []),
            LSP = gleam_lsp@languageserver@server:start(),
            IoInterface = {Read,ReadLine,Write},

            io:format("# gleam outliner: socket_receive_loop\n", []),
            spawn(fun() -> socket_receive_loop({LSP,Socket,IoInterface}) end),
            socket_accept_loop(Env);

        {error, closed} ->
            io:format("socket_accept_loop: closed\n", [])
    end.

open_socket(PORT) ->
    %try
        {ok, ListenSocket} = gen_tcp:listen(PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) ,
        socket_accept_loop(ListenSocket)
    %catch ECLS:ERR ->
    %    io:format("exception ~p\n",[ERR])
    %end
    .

open_lsp_socket(PORT) ->
    io:format("# gleam outliner: port ~p\n",[PORT]),
    open_socket(PORT).


to_int(X) when is_integer(X) -> X;
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_binary(X) -> to_int(binary_to_list(X));
to_int(X) when is_atom(X) -> to_int(atom_to_list(X));
to_int(X) -> X.

start([PORT])->
    io:format("# gleam outliner: starting language server: ~p\n",[PORT]),
    open_lsp_socket(to_int(PORT));

start(X)->
    io:format("# gleam outliner: starting language server: ~p\n",[X]),
    start([X]).

start()->
    start([55550]).
