-module(erlang_runtime).
-compile(export_all).

get_name(FullPath) ->
    BasenameWithExt = filename:basename(FullPath),
    filename:rootname(BasenameWithExt).

find(Dir) ->
    find(Dir, []).

find(Dir, Acc) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            lists:foldl(fun(Filename, Acc1) ->
                Path = filename:join([Dir, Filename]),
                case filelib:is_dir(Path) of
                    true  -> find(Path, Acc1);
                    false -> 
                        case filename:extension(Path) of
                            ".beam" -> [Path | Acc1];
                            _     -> Acc1
                        end
                end
            end, Acc, Filenames);
        {error, _Reason} -> Acc
    end.

load_all_modules(InitialModules) ->
    NeededModules = [type_conv | InitialModules],
    lists:foreach(
        fun(Module) ->
            try
                Res = code:ensure_loaded(Module),
                io:format("- Module: ~p\n",[Res])
            catch _:_ ->
                io:format("Failed to load module: ~p\n",[Module])
            end
        end,
        NeededModules),

    {ok, BaseDir} = file:get_cwd(),
    SrcFiles = find(BaseDir ++ "/ebin"),
    lists:foreach(
        fun(SrcFile) ->
            %io:format("SrcFile: ~p\n",[SrcFile]),
            ModuleName = get_name(SrcFile),
            Res = code:ensure_loaded( type_conv:to_atom(ModuleName) ),
            io:format("- Module: ~p\n",[Res])
        end,
        SrcFiles).

init_all() ->
    Loaded = code:all_loaded(),
    Modules = lists:map(fun({Module, _Path}) -> Module end, Loaded),
    lists:foreach(
        fun(Module) ->
            try
                Module:init(),
                io:format("- init: ~p\n",[Module])
            catch _:_ ->
                []
            end
        end,
        Modules
    ).