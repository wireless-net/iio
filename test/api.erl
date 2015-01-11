-module(api).

%% target API for the iio driver
test() ->
    {ok, [ChanA,...]} = iio:init_device("ad7091R2"),
    %% where ChanA = {ChanAName,ChanAIdx,ChanAType}
    %% 
    %% internally, search /sys/bus/iio/devices for matching device, if
    %% it exists, call iio:_init(Name, Num)

    ok = iio:enable_channel("ad7091R2", "in_voltage0"), % enables driver via sysfs, and tells lib that channel is enabled

    %% convenience function
    {ok, [{Trig1, Trig2, ...}]} = iio:list_triggers(),
    
    ok = iio:set_trigger("ad7091R2", "sysfstrig0"),

    ok = iio:set_buffer_length("ad7091R2", 100),

    %% will fail unless channels enabled, length set, and trigger set
    ok = iio:enable_buffer("ad7091R2"),

    %% will fail if channel not enabled
    {ok, Chan0} = iio:get_channel_handle("ad7091R2", "in_voltage0"),

    %% set the channel specific circular buffer length
    %% will fail if handle invalid
    ok = iio:set_chan_buffer_length(Chan0, 100),

    {ok, Data} = iio:read_channel(Chan0, 128),

    ok = iio:disable_buffer("ad7091R2"),

    ok = iio:set_trigger("ad7091R2","none"),	% dummy to discon.

    ok = iio:disable_channel("ad7091R2", "in_voltage0").


%% iio:enable_channel("ad7091R2", "in_voltage0")
%% 1. find _en file in /sys/bus/iio/devices/ 


% how to list all devices
% {ok,DevList} = file:list_dir("/sys/bus/i2c/devices").
% how to make a list of files to scan
%% when opening these name files, catch exceptions about file not existing, since not all entries under "devices" have name files.
% DevicesNames = lists:flatmap(fun(X) -> ["/sys/bus/iio/devices/" ++ X ++ "/name"] end, DevList).
%%
%% then we get the /dev entry by opening the "dev" file in the same directory where the name matches. This file contains something like: 253:X where the number after the ':' is the device minor number.
%%
%% Next we can construct the dev name as /dev/iio:deviceX
%%
%% Next we can open and read the in_T_scale file. Do this with a
%% list_dir followed my keyword search on "scale". The word before the
%% "scale" tells us the type of input: voltage, etc.
%%
%% Next make a list of scan_elements file (under the directory we were
%% in. Make a sublist of all files containing _en, _type, and _index files. 
