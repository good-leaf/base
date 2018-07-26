-module(bs_time).

%% API
-compile(export_all).

timestamp() ->
    {M, S, _MS} = os:timestamp(),
    M * 1000000 + S.

milltimestamp() ->
    {M, S, MS} = os:timestamp(),
    M * 1000000000 + S * 1000 + MS div 1000.

umilltimestamp() ->
    {M, S, MS} = os:timestamp(),
    M * 1000000000000 + S * 1000000 + MS.

date_format() ->
    Now = erlang:system_time(micro_seconds),
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1},{0, 0, 0}}),
    Time = calendar:gregorian_seconds_to_datetime(BaseDate + Now div 1000000),
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time_to_local_time(Time),
    US = Now rem 1000000 div 1000,
    list_to_binary(io_lib:format("~4..0b~2..0b~2..0b~2..0b~2..0b~2..0b~3..0b",[Y, M, D, H, Mi, S, US])).

date_format2() ->
    Now = erlang:system_time(micro_seconds),
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970, 1, 1},{0, 0, 0}}),
    Time = calendar:gregorian_seconds_to_datetime(BaseDate + Now div 1000000),
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time_to_local_time(Time),
    US = Now rem 1000000,
    list_to_binary(io_lib:format("~4..0b~2..0b~2..0b~2..0b~2..0b~2..0b~6..0b",[Y, M, D, H, Mi, S, US])).

