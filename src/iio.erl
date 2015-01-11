%%%-------------------------------------------------------------------
%%% @author Devin Butterfield <>
%%% @copyright (C) 2015, Devin Butterfield
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2015 by Devin Butterfield <>
%%%-------------------------------------------------------------------
-module(iio).

-behaviour(gen_server).

%% API
-export([start_link/0, init_device/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% A place to keep the IIO library handle...
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

init_device(DevName) ->
    gen_server:call(?MODULE, {init_device, DevName}).

%% enable_channel(DevName, ChanName) ->
%%     gen_server:call(?MODULE, {enable_channel, DevName, ChanName}).


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
handle_call({init_device, DevName}, _From, State) ->
    %% grab the device information given the name
    DevInfo = get_device_info(DevName),
    Reply = iio_nif:initialize(DevInfo),
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
find_name_match(_MatchName,[]) ->
    not_found;
find_name_match(MatchName, [Path|PathList]) ->
    NameFile = file:open(Path,[read]),
    NameString = file:read_line(NameFile),
    case MatchName == NameString of
	true ->
	    file:close(NameFile),
	    Path;
	false ->
	    file:close(NameFile),
	    find_name_match(MatchName, PathList)
    end.

get_index_list(Path, [], IndexList) ->
    lists:reverse(IndexList);
get_index_list(Path, [IndexFile|IndexFileList], IndexList) ->
    File = file:open(Path ++ "/scan_elements/" ++ IndexFile,[read]),
    IndexString = file:read_line(File),
    Index = list_to_integer(IndexString),
    get_index_list(Path, IndexFileList, [Index|IndexList]).

get_minor(Path) ->
    DevPath = Path ++ "/dev",
    DevFile = file:open(DevPath,[read]),
    DevString = file:read_line(DevFile),
    string:substr(DevString, string:str(DevString, ":") + 1).
    %list_to_integer(Minor).

sublist_by_key(Word, List) ->
    lists:filter(fun(Desc) -> 
			 string:str(Desc, Word) > 0 end, List).

get_channel_info(Path) ->
    ScanElementFiles = file:list_dir(Path ++ "/scan_elements"),
    %% ChanEnableFileList = sublist_by_key("_en", ScanElementFiles),
    ChanIndexFileList = sublist_by_key("_index", ScanElementFiles),
    ChanNameList = lists:flatmap(fun(Desc) -> [string:substr(Desc, 1, string:str(Desc, "_index"))] end, ChanIndexFileList),
    ChanIndexList = get_index_list(Path, ChanIndexFileList, []),
    lists:zip3(ChanNameList, ChanIndexList).

get_device_info(Name) ->
    {ok,DevList} = file:list_dir("/sys/bus/iio/devices"),
    DevicePaths = lists:flatmap(fun(X) -> ["/sys/bus/iio/devices/" ++ X] end, DevList),
    PathMatch = find_name_match(Name, DevicePaths),
    Minor = get_minor(PathMatch),
    ChanInfo = get_channel_info(PathMatch),
    {Name, "/dev/iio:device" ++ Minor, PathMatch, ChanInfo}.
    
	
