iio
===

**iio** is an Erlang NIF library that provides a low overhead inteface to Linux IIO (industrial-IO) driver framework devices. This NIF library was written to allow Erlang applications to efficiently read ADC data from the AD7091R-2 (12-bit 1MSps) device on the Lumenosys Robotics [BMOD][1] board. You can use this NIF libraray to easily read analog voltages and sensors using the BMOD.

Dependencies
------------

To build you will need a working installation of Erlang 17 (or
later). <br/>
Please refer to [Erlang/OTP](http://www.erlang.org) for information on building and installing Erlang/OTP.

This application is built using [rebar](https://github.com/rebar/rebar). Refer to [building rebar](https://github.com/rebar/rebar/wiki/Building-rebar) for information on building and using rebar.

Downloading
-----------

```sh
$ git clone git://github.com/lumenosys/iio.git
```
Building
--------

Compile:

```sh
$ cd iio
$ make all
...
==> iio (compile)
```

Usage example
-------------

```erlang
	%%
	%% Code snippet from the BMOD bsp (not complete)
	%%

    %% Initialize the ad7091R-2 ADC device
    iio:device_init("ad7091R2"),

    %% set the trigger frequency to 1Hz
    {ok,File} = file:open("/sys/bus/iio/devices/trigger0/frequency", [write]),
    file:write(File, integer_to_list(1) ++ "\n"),  
    file:close(File),

    %% get the scale value
    {ok,File2} = file:open("/sys/bus/iio/devices/iio:device0/in_voltage_scale", [read]),
    {ok,ScaleStr} = file:read_line(File2),
    Scale = list_to_float(string:strip(ScaleStr, right, $\n)),
    file:close(File2),

    %% init the iio channel 0 
    iio:channel_enable("ad7091R2", "in_voltage0"),

    %% setup the ring buffer
    iio:buffer_set_length("ad7091R2", 10),

    %% connect the 1Hz trigger
    iio:trigger_connect("ad7091R2", "bfintmr5"),

    %% start it!
    iio:buffer_enable("ad7091R2"),

    %% grab handle
    {ok, Chan0} = iio:channel_get_handle("ad7091R2", "in_voltage0"),

    %% some time later
    case iio:channel_read(Chan0, 1) of
        {ok,<<Sample:16/unsigned-big-integer, _Rest/binary>>} ->
            Volts = (Sample band 16#fff) * Scale * ?BATT_CAL,
            Status = check_battery(Volts, Sum),
            batt_status_send(Status, Count),
            io:format("Volts = ~p Sum=~p~n",[Volts,Sum]),
            batt_mon_loop(Chan, Scale, Sum + Volts, Count + 1);
        rx_queue_empty ->
            batt_mon_loop(Chan, Scale, Sum, Count)
    end

```

Copyright and License
---------------------

> %CopyrightBegin%
>
> Copyright Lumenosys Robotics 2014-2015. All Rights Reserved.
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
>     http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.
>
> %CopyrightEnd%


[1]: https://lumenosys.com/products