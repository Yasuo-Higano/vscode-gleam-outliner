-module(erlang_test).
-compile(export_all).

%    F = fun rfun(X) ->
%            case X of
%                0 -> [0];
%                A -> [A | rfun(A-1)]
%            end,
%    F = rfun(X) ->
%            case X of
%                0 -> [0];
%                A -> [A | rfun(A-1)]
%            end,
% 


test_rec1() ->
    F = fun(Self,X) ->
            case X of
                0 -> [0];
                A -> [A | Self(Self,A-1)]
            end
        end,
    R = F(F,10),
    io:format("rfun = ~p\n",[R]).

%test_rec2() ->
%    F = fun frec(X) ->
%            case X of
%                0 -> [0];
%                A -> [A | frec(A-1)]
%            end
%        end,
%    R = F(10),
%    io:format("rfun = ~p\n",[R]).

%test_rec3() ->
%    F = fun(X) ->
%            Self = fun(X) ->
%                case X of
%                    0 -> [0];
%                    A -> [A | Self(A-1)]
%                end
%            end
%        end,
%    R = (F(F))(10),
%    io:format("rfun = ~p\n",[R]).

test_pattern_match() ->
    case [1,2,3] of 
        [2-1,4-2,6-3] -> io:format("pattern match 1 ok\n")
        end,

    LST = [1,2,3],
    LST2 = [2,3,4],
    case [1,2,3] of 
        LST2 -> io:format("pattern match 2 FAILED: ~p\n",[LST2]);
        LST -> io:format("pattern match 2 ok: ~p\n",[LST]);
        Other -> io:format("pattern match 2 FAILED\n")
        end,
    true.

test() ->
    io:format("ERLANG TEST ========================================\n",[]),
    test_pattern_match(),
    test_rec1().
    %test_rec2(),
    %test_rec3().
