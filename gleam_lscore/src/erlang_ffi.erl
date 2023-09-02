-module(erlang_ffi).
-compile(export_all).

log(Fmt,Params)->
   if
      is_tuple(Params) -> io:format(standard_error, Fmt,tuple_to_list(Params));
      true -> io:format(standard_error, Fmt,Params)
   end,
   io:format(standard_error, "\n",[]).

symbol_of(Str) ->
   {ok, Atom} = binary_to_atom(Str),
   Atom.

untyped_of(X) -> X.

catch_exception(F) ->
   try
      {ok, F()}
   catch
      throw:ERR ->
         {error,ERR};
         
      error:ERR ->
         %io:format("**** Exceptinon: ~p\n",[ERR]),
         {error, ERR};

      A:B ->
         io:format("**** UNHANDABLE Exceptinon: ~p\n ~p\n",[A,B]),
         {error, B}
   end.

throw_exception(E) ->
   throw(E).

box(Val) when is_integer(Val) -> {int_val, Val};
box(Val) when is_float(Val) -> {float_val, Val};
box(Val) when is_binary(Val) -> {string_val, Val};
box(Val) when is_boolean(Val) -> {boolean, Val};
box(Val) when is_atom(Val) -> {atom_val, Val};
box(Val) -> Val.

unbox(Expr) ->
   case Expr of
      {int_val,Val} -> Val;
      {float_val,Val} -> Val;
      {string_val,Val} -> Val;
      {boolean,Val} -> Val;
      {atom_val,Val} -> Val;
      _ -> Expr
   end.

ets_keys(Tab) ->
   ets:foldl(fun({Key,_},Acc)->[Key | Acc] end,[],Tab).

call_method(Method,Self,Params) ->
   apply(Method,[Self | Params]).

println(Msg) ->
   %io:format("~p | ~s\n",[Msg, Msg]),
   io:format("~s\n",[
      color:rgb([2,2,2],io_lib:format("~p",[Msg]))
      ]),
   Msg.

printp(Msg) ->
   io:format("~s\n",[
      color:rgb([2,2,2],io_lib:format("~p",[Msg]))
   ]),
   Msg.

debug_assert_true(Bool) ->
   if Bool == true -> true;
      true -> throw_exception(<<"assertion_failed">>), false
   end.

debug_assert_equal(A,B) ->
   try
      A = B,
      io:format("~p ~p -----------------------------------------------------\n\n",[A,B])
   catch _:_ ->
      io:format("~p ~p #####################################################\n\n",[A,B])
   end.


debug_assert_false(Bool) ->
   if Bool == false -> true;
      true -> throw_exception(<<"assertion_failed">>), false
   end.


test(A1) ->
   io:format("***** HELLLOOOOO ~p\n\n",[A1]),
   list_to_binary(io_lib:format("TEST STRING + ~p",[A1])).


promise(ExecutorFun) ->
    Tag = erlang:make_ref(),
    Self = self(),
    WorkerFun = fun() ->
        % Do the work

        Response = try ExecutorFun() of
            Result ->
                % If everything goes as planned just return an `ok` tuple with the response
                {ok, Result}
            catch
                Error:Reason ->
                    % Otherwise return an error tuple with the exception type and reason
                    {error, {promise_exception,Error,Reason}}
        end,

        % Send a message with the result back to the callers process
        Self ! {Tag, Response}
    end,
    spawn(WorkerFun),

    % Return the reference so we can use it in the yield function
    {Tag, Self}.

yield({Tag,SpawnByPid}) ->
   SpawnByPid = self(),
    receive
        {Tag, Response} ->
            Response
    end.


make_promise(ExecutorFun) ->
   Tag = promise(ExecutorFun),
   fun()-> yield(Tag) end.

trim_string(Str)->
   if is_list(Str) -> list_to_binary(Str);
      true -> Str
   end.

complete_str(All,[],[],Len) ->
   "";

complete_str(All,L,[],Len) ->
   "";

complete_str(All,[],Str,Len) ->
   %io:format("New Loop ~p",[Str]),
   [_ | A] = lists:reverse(Str),
   complete_str(All,All,lists:reverse(A),string:length(A));

complete_str(All,[L | Rest],Str,Len) ->
   S = string:slice(L, 0, Len),
   %io:format(" - ~s ~s\n",[S,Str]),
   case S == Str of
      true -> string:slice(L,Len);
      _ -> complete_str(All,Rest,Str,Len)
   end.

%init_device_as_binary_mode(Dev) ->
%   io:setopts(
%      Dev,
%      %[binary,{encoding,utf8},{echo,false}]
%      [binary,{encoding,utf8}]
%   ).
%
%init_stdio() ->
%   init_device_as_binary_mode(standard_io),
%   init_device_as_binary_mode(standard_error).

init_io(Candidates) ->
   Ex = fun(Binstr) ->
      %Str = case is_binary(Binstr) of
      %   true -> binary_to_list(Binstr);
      %   _ -> lists:reverse(Binstr)
      %end,
      Str = lists:reverse(Binstr),
      %L = [
      %   "list_vars()",
      %   "list_types()"
      %],
      L = lists:map(fun(Str)-> binary_to_list(Str) end,Candidates),
      case complete_str(L,L,Str,string:length(Str)) of
         "" -> {no, "", L};
         X -> {yes,X,[]}
      end
   end,
   io:setopts(
      [binary,{encoding,utf8},{echo,true},{expand_fun,Ex}]
   ).

unicode_next_codepoint(Str) ->
   case string:next_codepoint(Str) of
      [] -> none;
      [Charcode|Rest]  -> {some, {Charcode,Rest}}
   end.

decode_json(Src) ->
   Obj = jsone:decode(Src,[{object_format, tuple}]),
   % io:format(standard_error,"json obj = ~p\n",[Obj]),
   cnvgjson(Obj).

cnvgjson_obj({Objs}) ->
   lists:map(fun({K,V}) ->
                   {j_pair,K,cnvgjson(V)}
             end,Objs).

cnvgjson(Obj) ->
   if
      is_list(Obj) -> {j_list,Obj};
      is_integer(Obj) -> {j_int,Obj};
      is_float(Obj) -> {j_float,Obj};
      is_binary(Obj) -> {j_str,Obj};
      is_tuple(Obj) -> {j_object,cnvgjson_obj(Obj)};
      true -> j_true;
      false -> j_false;
      null -> j_null;
      true -> io:format(standard_error,"???? ~p\n",[Obj])
   end.
      
to_string(X) ->
   if
      is_integer(X) -> io_lib:format("~p",[X]);
      is_atom(X) -> io_lib:format("~s",[X]);
      is_binary(X) -> io_lib:format("~s",[X]);
      true -> io_lib:format("~p",[X])
   end.
