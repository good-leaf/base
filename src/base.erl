-module(base).

-compile(export_all).

random_seed() ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()).

-spec random(integer()) -> integer().
random(Scope) ->
    random:uniform(Scope).

sequence() ->
    integer_to_binary(erlang:unique_integer([positive, monotonic])).

generate_uuid() ->
    Bin = crypto:strong_rand_bytes(12),
    Time = integer_to_binary(bs_time:umilltimestamp()),
    NewBin = <<Bin/binary, Time/binary>>,
    Sig = erlang:md5(NewBin),
    iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)]).
