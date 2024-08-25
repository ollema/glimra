-module(libglans).

-export([truly_random/0]).
-nifs([truly_random/0]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif(filename:join([code:priv_dir("glans"), "libglans"]), 0).

truly_random() ->
    erlang:nif_error(nif_library_not_loaded).
