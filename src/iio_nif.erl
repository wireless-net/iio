%%% @author Lumenosys Robotics <dbutter@lumenosys.com>
%%% @copyright (C) 2014, Lumenosys Robotics
%%% @doc
%%%

-module(iio_nif).
-export([initialize/1, 
	 channel_enable/2,
	 channel_disable/2,
	 channel_get_handle/2,
	 channel_buffer_set_length/2,
	 channel_read/2,
	 buffer_enable/2,
	 buffer_disable/1,
	 buffer_set_length/2
	]).

-on_load(init/0).

init() ->
    Lib = filename:join(code:priv_dir("iio"), "libiio_nif"),
    ok = erlang:load_nif(Lib, 0).

initialize(_DevName) ->
    exit(nif_library_not_loaded).

channel_enable(_DevName, _ChanName) ->
    exit(nif_library_not_loaded).

channel_disable(_DevName, _ChanName) ->
    exit(nif_library_not_loaded).

channel_get_handle(_DevName, _ChanName) ->
    exit(nif_library_not_loaded).

channel_buffer_set_length(_ChanHandle, _Len) ->
    exit(nif_library_not_loaded).

channel_read(_ChanHandle, _Len) ->
    exit(nif_library_not_loaded).

buffer_enable(_DevName, _BufDevPath) ->
    exit(nif_library_not_loaded).

buffer_disable(_DevName) ->
    exit(nif_library_not_loaded).

buffer_set_length(_DevName, _Len) ->
    exit(nif_library_not_loaded).

%%% @end
