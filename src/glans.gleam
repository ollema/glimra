import gleam/io

@external(erlang, "libglans", "truly_random")
pub fn truly_random() -> String

pub fn main() {
  io.debug(truly_random())
}
