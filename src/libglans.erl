-module(libglans).
-export([get_supported_languages/0, get_highlight_name/1, get_highlight_events/2]).
-nifs([get_supported_languages/0, get_highlight_name/1, get_highlight_events/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif(nif_filepath(), 0).

nif_filepath() ->
    DllName = case {os(), arch()} of
        {macos, aarch64} -> "libglans-macos-aarch64";
        {macos, x86_64} -> "libglans-macos-x86_64";
        {linux, x86_64} -> "libglans-linux-x86_64";
        {windows, x86_64} -> "libglans-windows-x86_64";
        {Os, Arch} -> throw({dll_not_found, ["Unsupported platform", Os, Arch]})
    end,
    filename:join([code:priv_dir("glans"), "lib", DllName]).

os() ->
    case os:type() of
        {unix, linux} -> linux;
        {unix, darwin} -> macos;
        {win32, nt} -> windows;
        {_, Other} -> {other, atom_to_binary(Other, utf8)}
    end.

arch() ->
    SystemArchitecture = erlang:system_info(system_architecture),
    case string:split(SystemArchitecture, "-") of
        ["x86_64", _] -> x86_64;
        ["aarch64", _] -> aarch64;
        ["win32"] -> x86_64;
        Other -> {other, Other}
    end.

get_supported_languages() ->
    erlang:nif_error(nif_library_not_loaded).

get_highlight_name(_Index) ->
    erlang:nif_error(nif_library_not_loaded).

get_highlight_events(_Source, _Language) ->
    erlang:nif_error(nif_library_not_loaded).
