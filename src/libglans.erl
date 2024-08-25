-module(libglans).
-export([truly_random/0]).
-nifs([truly_random/0]).
-on_load(init/0).

init() ->
    io:format("loading libglans...~n"),
    ok = erlang:load_nif("priv/libglans", 0),
    io:format("loaded libglans.~n").
    

truly_random() ->
    exit(nif_library_not_loaded).