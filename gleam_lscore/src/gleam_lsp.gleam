import gleam_lsp/languageserver/server

@external(erlang, "gleam_lsp_sock", "start")
pub fn start_sock() -> Nil

@external(erlang, "gleam_lsp_stdio", "start")
pub fn start_stdio() -> Nil

pub fn start() {
  server.init_module()
  start_stdio()
}
