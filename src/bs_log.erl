-module(bs_log).

-compile(export_all).

log_trace(FileName) ->
    FileSize = application:get_env(lager, file_size, 100),
    FileNum = application:get_env(lager, file_num, 10),
    lager_trace(log1_lager_event, log1, FileName, FileSize, FileNum, "1"),
    lager_trace(log2_lager_event, log2, FileName, FileSize, FileNum, "2"),
    lager_trace(log3_lager_event, log3, FileName, FileSize, FileNum, "3"),
    lager_trace(log4_lager_event, log4, FileName, FileSize, FileNum, "4"),
    lager_trace(log5_lager_event, log5, FileName, FileSize, FileNum, "5").

lager_trace(Sink, Type, FileName, FileSize, Count, Index) ->
    {ok, Service} = inet:gethostname(),
    AppKey = application:get_env(lager, app_key, "app_key"),
    LogEnv = application:get_env(lager, log_env, "log_env"),
    FileDir = application:get_env(lager, file_dir, ""),
    lager:trace_file(FileDir ++ "/" ++ atom_to_list(FileName) ++ Index ++ ".log",
        [{Type, FileName}, {sink, Sink}], info,
        [
            {size, FileSize * 1024 * 1024},
            {count, Count},
            {formatter_config, [date, " ", time, " ",
                Service, " ", AppKey, " [info] ",
                atom_to_list(FileName), " ", LogEnv, " ", message, "\n"]
            }
        ]).

hash_log(HashKey, FileName, Format, Val) ->
    log_msg(erlang:phash2(HashKey, 5) + 1, FileName, Format, Val).

log_msg(0, Name, Format, Val) -> log1:info([{log1, Name}], Format, Val);
log_msg(1, Name, Format, Val) -> log2:info([{log2, Name}], Format, Val);
log_msg(2, Name, Format, Val) -> log3:info([{log3, Name}], Format, Val);
log_msg(3, Name, Format, Val) -> log4:info([{log4, Name}], Format, Val);
log_msg(4, Name, Format, Val) -> log5:info([{log5, Name}], Format, Val).

generate_kv(DataList) ->
    Kv = lists:foldl(fun(T, <<>>) ->
        {K, V} = T,
        BK = to_binary(K),
        BV = to_binary(V),
        <<BK/binary, "=", BV/binary>>;
        (T, Acc) ->
            {K, V} = T,
            BK = to_binary(K),
            BV = to_binary(V),
            <<Acc/binary, " ", BK/binary, "=", BV/binary>> end, <<>>, DataList),
    <<"#XMDT#{", Kv/binary, "}#XMDT#">>.

to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_float(X) -> float_to_binary(X);
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_list(X) ->
    case io_lib:printable_list(X) of
        true ->
            list_to_binary(X);
        false ->
            term_to_binary(X)
    end.