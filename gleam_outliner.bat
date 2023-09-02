@cd /d %~dp0 > nul
@erl -pa ebin -s gleam_lsp start -noshell
