%%%-------------------------------------------------------------------
%%% @author Lumenosys Robotics <>
%%% @copyright (C) 2015, Lumenosys Robotics
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2015 by Lumenosys Robotics <>
%%%-------------------------------------------------------------------
-module(iio).

-behaviour(gen_server).

%% API
-export([start_link/0, 
	 device_init/1, 
	 channel_enable/2,
	 channel_disable/2,
	 channel_get_handle/2,
	 channel_buffer_set_length/2,
	 channel_read/2,
	 trigger_connect/2,
	 trigger_disconnect/1,
	 buffer_set_length/2,	 
	 buffer_enable/1,
	 buffer_disable/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% A place to keep the IIO library handle...
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Initialize the IIO NIF library for a specified IIO device
%% @end
-spec device_init(DevName) -> Reply when
    DevName :: string(),
    Reply :: term().

device_init(DevName) ->
    gen_server:call(?MODULE, {device_init, DevName}).

%% @doc Enable the specified IIO channel for the specified IIO device
%% @end
-spec channel_enable(DevName, ChanName) -> Reply when
    DevName :: string(),
    ChanName :: string(),
    Reply :: term().

channel_enable(DevName, ChanName) ->
    gen_server:call(?MODULE, {channel_enable, DevName, ChanName}).

%% @doc Disable the specified IIO channel for the specified IIO device
%% @end
-spec channel_disable(DevName, ChanName) -> Reply when
    DevName :: string(),
    ChanName :: string(),
    Reply :: term().

channel_disable(DevName, ChanName) ->
    gen_server:call(?MODULE, {channel_disable, DevName, ChanName}).

%% @doc Get opaque channel handle for IO operations with the NIF library
%% @end
-spec channel_get_handle(DevName, ChanName) -> Reply when
    DevName :: string(),
    ChanName :: string(),
    Reply :: term().

channel_get_handle(DevName, ChanName) ->
    gen_server:call(?MODULE, {channel_get_handle, DevName, ChanName}).

%% @doc Set NIF library internal channel circular buffer depth for specified channel
%% @end
-spec channel_buffer_set_length(ChannelHandle, Len) -> 'ok' | 'alloc_failed' when
    ChannelHandle :: term(),
    Len :: integer().

channel_buffer_set_length(ChanHandle, Len) ->
    iio_nif:channel_buffer_set_length(ChanHandle, Len).

%% @doc Read Len samples of data from channel specified by handle
%% @end
-spec channel_read(ChannelHandle, Len) -> 'buffer_not_enabled' | 'channel_not_enabled' | 'alloc_failure' | 'rx_queue_empty' | {ok, Data} | {'error', ErrType} when
    ChannelHandle :: term(),
    Len :: integer(),
    Data :: binary(),
    ErrType :: atom().

channel_read(ChanHandle, Len) ->
    iio_nif:channel_read(ChanHandle, Len).

%% @doc Connect specified IIO device to specified IIO trigger 
%% @end
-spec trigger_connect(DevName, TrigName) -> Reply when
    DevName :: string(),
    TrigName :: string(),
    Reply :: term().

trigger_connect(DevName, TrigName) ->
    gen_server:call(?MODULE, {trigger_connect, DevName, TrigName}).

%% @doc Disconnect the specified IIO device from the current trigger
%% @end
-spec trigger_disconnect(DevName) -> Reply when
    DevName :: string(),
    Reply :: term().

trigger_disconnect(DevName) ->
    gen_server:call(?MODULE, {trigger_disconnect, DevName}).

%% @doc Set IIO device kernel buffer length (samples)
%% @end
-spec buffer_set_length(DevName, Len) -> Reply when
    DevName :: string(),
    Len :: integer(),
    Reply :: term().

buffer_set_length(DevName, Len) ->
    gen_server:call(?MODULE, {buffer_set_length, DevName, Len}).

%% @doc Enable the IIO buffer for the specified IIO device
%% @end
-spec buffer_enable(DevName) -> Reply when
    DevName :: string(),
    Reply :: term().

buffer_enable(DevName) ->
    gen_server:call(?MODULE, {buffer_enable, DevName}).

%% @doc Disable the buffer for the specified IIO device
%% @end
-spec buffer_disable(DevName) -> Reply when
    DevName :: string(),
    Reply :: term().

buffer_disable(DevName) ->
    gen_server:call(?MODULE, {buffer_disable, DevName}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({device_init, DevName}, _From, State) ->
    %% grab the device information given the name
    DevInfo = get_device_info(DevName),
    Reply = iio_nif:initialize(DevInfo),
    {reply, Reply, State};
handle_call({channel_enable, DevName, ChanName}, _From, State) ->
    %% lookup the device information
    {_Name, _BufDev, Path, _ChanInfo} = get_device_info(DevName),
    {ok,File} = file:open(Path ++ "/scan_elements/" ++ ChanName ++ "_en", [write]),
    ok = file:write(File, "1"),
    file:close(File),
    Reply = iio_nif:channel_enable(DevName, ChanName),
    {reply, Reply, State};
handle_call({channel_disable, DevName, ChanName}, _From, State) ->
    %% lookup the device information
    {_Name, _BufDev, Path, _ChanInfo} = get_device_info(DevName),
    {ok,File} = file:open(Path ++ "/scan_elements/" ++ ChanName ++ "_en", [write]),
    ok = file:write(File, "0"),
    file:close(File),
    Reply = iio_nif:channel_disable(DevName, ChanName),
    {reply, Reply, State};
handle_call({channel_get_handle, DevName, ChanName}, _From, State) ->
    Reply = iio_nif:channel_get_handle(DevName, ChanName),
    {reply, Reply, State};
handle_call({trigger_connect, DevName, TrigName}, _From, State) ->
    %% lookup the device information
    {_Name, _BufDev, Path, _ChanInfo} = get_device_info(DevName),
    {ok,File} =file:open(Path ++ "/trigger/current_trigger", [write]),
    ok = file:write(File, TrigName),
    Reply = file:close(File),
    {reply, Reply, State};
handle_call({trigger_disconnect, DevName}, _From, State) ->
    %% lookup the device information
    {_Name, _BufDev, Path, _ChanInfo} = get_device_info(DevName),
    {ok,File} =file:open(Path ++ "/trigger/current_trigger", [write]),
    ok = file:write(File, "none"),
    Reply = file:close(File),
    {reply, Reply, State};
handle_call({buffer_set_length, DevName, Len}, _From, State) ->
    %% lookup the device information
    {_Name, _BufDev, Path, _ChanInfo} = get_device_info(DevName),
    {ok,File} =file:open(Path ++ "/buffer/length", [write]),
    ok = file:write(File, integer_to_list(Len)),
    ok = file:close(File),
    Reply = iio_nif:buffer_set_length(DevName, Len),
    {reply, Reply, State};
handle_call({buffer_enable, DevName}, _From, State) ->
    %% lookup the device information
    {_Name, BufDev, Path, _ChanInfo} = get_device_info(DevName),
    %% workaround for device files not getting created when driver is
    %% statically compiled into kernel
    {Major, Minor} = get_cdev(Path),
    sys_cmd("/bin/mknod", [BufDev, "-m", "666", "c", Major, Minor]), 
    %% ok, enable it
    {ok,File} =file:open(Path ++ "/buffer/enable", [write]),
    ok = file:write(File, "1"),
    file:close(File),
    Reply = iio_nif:buffer_enable(DevName, BufDev),
    {reply, Reply, State};
handle_call({buffer_disable, DevName}, _From, State) ->
    %% lookup the device information
    {_Name, _BufDev, Path, _ChanInfo} = get_device_info(DevName),
    {ok,File} =file:open(Path ++ "/buffer/enable", [write]),
    ok = file:write(File, "0"),
    file:close(File),
    Reply = iio_nif:buffer_disable(DevName),
    {reply, Reply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% run a system command (like unix:cmd() but doesn't require fully
%% functional shell)
sys_cmd(Cmd, Args) ->
    CmdPort = open_port({spawn_executable, Cmd}, 
			[{args, Args}, exit_status]),
    receive
	{CmdPort, {data, StdOut}} ->
	    io:format("~p",[StdOut]);
	{CmdPort, {exit_status, Status}} ->
	    io:format("~p~n", [Status]),
	    Status
    end.

find_name_match(_MatchName,[]) ->
    not_found;
find_name_match(MatchName, [Path|PathList]) ->
    case file:open(Path ++ "/name" ,[read]) of
	{ok,NameFile} -> 
	    {ok,NameString} = file:read_line(NameFile),
	    case (MatchName ++ "\n") == NameString of
		true ->
		    file:close(NameFile),
		    Path;
		false ->
		    file:close(NameFile),
		    find_name_match(MatchName, PathList)
	    end;
	{error,enoent} ->
	    find_name_match(MatchName, PathList)
    end.
		

get_index_list(_Path, [], IndexList) ->
    lists:reverse(IndexList);
get_index_list(Path, [IndexFile|IndexFileList], IndexList) ->
    {ok,File} = file:open(Path ++ "/scan_elements/" ++ IndexFile,[read]),
    {ok,IndexString} = file:read_line(File),
    Index = list_to_integer(string:strip(IndexString, right, $\n)),
    file:close(File),
    get_index_list(Path, IndexFileList, [Index|IndexList]).

get_cdev(Path) ->
    DevPath = Path ++ "/dev",
    {ok,DevFile} = file:open(DevPath,[read]),
    {ok,DevString} = file:read_line(DevFile),
    file:close(DevFile),
    DevStr = string:strip(DevString, right, $\n),
    Major = string:substr(DevStr, 1, string:str(DevStr, ":") - 1),
    Minor = string:substr(DevStr, string:str(DevStr, ":") + 1),
    {Major, Minor}.

sublist_by_key(Word, List) ->
    lists:filter(fun(Desc) -> 
			 string:str(Desc, Word) > 0 end, List).

get_channel_info(Path) ->
    {ok,ScanElementFiles} = file:list_dir(Path ++ "/scan_elements"),
    ChanIndexFileList = sublist_by_key("_index", ScanElementFiles),
    ChanNameList = lists:flatmap(fun(Desc) -> [string:substr(Desc, 1, string:str(Desc, "_index") - 1)] end, ChanIndexFileList),
    ChanIndexList = get_index_list(Path, ChanIndexFileList, []),
    lists:zip(ChanNameList, ChanIndexList).

get_device_info(Name) ->
    {ok,DevList} = file:list_dir("/sys/bus/iio/devices"),
    DevicePaths = lists:flatmap(fun(X) -> ["/sys/bus/iio/devices/" ++ X] end, DevList),
    PathMatch = find_name_match(Name, DevicePaths),
    {_Major, Minor} = get_cdev(PathMatch),
    ChanInfo = get_channel_info(PathMatch),
    {Name, "/dev/iio:device" ++ Minor, PathMatch, ChanInfo}.
    
	
