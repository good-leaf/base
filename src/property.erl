-module(property).

-compile(export_all).

-define(APPS, [inets, kernel]).

memory_sort(Num) ->
    ProcessL = processes() -- [self()],
    AttrName = memory,
    F = fun(Pid, L) ->
        case process_info(Pid, [AttrName, registered_name, current_function, initial_call]) of
            [Attr, Name, Init, Cur] ->
                Info = {Attr, [{pid, Pid}, Name, Init, Cur]},
                [Info | L];
            undefined ->
                L
        end
        end,
    ProInfoL = lists:foldl(F, [], ProcessL),
    CompF = fun({A, _},{B, _}) ->
        A > B
            end,
    ProInfoSortL = lists:usort(CompF, ProInfoL),
    lists:sublist(ProInfoSortL, Num).

%系统限制
system() ->
    string:tokens(binary_to_list(erlang:system_info(info)), "\n").

%最大ets表
ets_memory() ->
    lists:reverse(lists:keysort(2, [{T, ets:info(T, memory)} || T <- ets:all()])).

cpu_spend() ->
    lists:reverse(lists:sort([{process_info(X, reductions), X} || X <- processes()])).

queue_len() ->
    lists:reverse(lists:sort([{process_info(X, message_queue_len), X} || X <- processes()])).

eprof_start() ->
    eprof:start(),
    case lists:keyfind(running, 1, application:info()) of
        {_, Apps} ->
            case get_procs(?APPS, Apps) of
                [] ->
                    {error, no_procs_found};
                Procs ->
                    eprof:start_profiling(Procs)
            end;
        _ ->
            {error, no_app_info}
    end.

fprof_start() ->
    fprof_start(0).

fprof_start(Duration) ->
    case lists:keyfind(running, 1, application:info()) of
        {_, Apps} ->
            case get_procs(?APPS, Apps) of
                [] ->
                    {error, no_procs_found};
                Procs ->
                    fprof:trace([start, {procs, Procs}]),
                    io:format("Profiling started~n"),
                    if Duration > 0 ->
                        timer:sleep(Duration * 1000),
                        fprof:trace([stop]),
                        fprof:stop();
                        true ->
                            ok
                    end
            end;
        _ ->
            {error, no_app_info}
    end.

fprof_stop() ->
    fprof:trace([stop]),
    fprof:profile(),
    fprof:analyse([totals, no_details, {sort, own},
        no_callers, {dest, "fprof.analysis"}]),
    fprof:stop(),
    format_fprof_analyze().

fprof_analyze() ->
    fprof_stop().

eprof_stop() ->
    eprof:stop_profiling(),
    eprof:analyze().

queue(N) ->
    dump(N, lists:reverse(lists:ukeysort(1, all_pids(queue)))).

memory(N) ->
    dump(N, lists:reverse(lists:ukeysort(3, all_pids(memory)))).

reds(N) ->
    dump(N, lists:reverse(lists:ukeysort(4, all_pids(reductions)))).

gc(Pid) ->
    erlang:garbage_collect(Pid).

auto_gc(MemoryLimit) ->
    auto_gc(MemoryLimit, 5).

auto_gc(MemoryLimit, ProcessNum) ->
    ProInfo = memory_sort(ProcessNum),
    Fun = fun(T) ->
        {{memory,Memory}, [{pid,Pid} | _Tuple]} = T,
        case Memory div 1000 div 1000 > MemoryLimit of
            true ->
                gc(Pid);
            false ->
                ok
        end end,
    [Fun(T) || T <- ProInfo].

%%====================================================================
%% Internal functions
%%====================================================================
get_procs(Apps, AppList) ->
    io:format("Searching for processes to profile...~n", []),
    Procs = lists:foldl(
        fun({App, Leader}, Acc) when is_pid(Leader) ->
            case lists:member(App, Apps) of
                true ->
                    get_procs2(Leader) ++ Acc;
                false ->
                    Acc
            end;
            (_, Acc) ->
                Acc
        end, [], AppList),
    io:format("Found ~p processes~n", [length(Procs)]),
    Procs.

get_procs2(Leader) ->
    lists:filter(
        fun(Pid) ->
            case process_info(Pid, group_leader) of
                {_, Leader} ->
                    true;
                _ ->
                    false
            end
        end, processes()).

format_fprof_analyze() ->
    case file:consult("fprof.analysis") of
        {ok, [_, [{totals, _, _, TotalOWN}] | Rest]} ->
            OWNs =
                lists:flatmap(
                    fun({MFA, _, _, OWN}) ->
                        Percent = OWN * 100 / TotalOWN,
                        case round(Percent) of
                            0 -> [];
                            _ -> [{mfa_to_list(MFA), Percent}]
                        end
                    end, Rest),
            ACCs = collect_accs(Rest),
            MaxACC = find_max(ACCs),
            MaxOWN = find_max(OWNs),
            io:format("=== Sorted by OWN:~n"),
            lists:foreach(
                fun({MFA, Per}) ->
                    L = length(MFA),
                    S = lists:duplicate(MaxOWN - L + 2, $ ),
                    io:format("~s~s~.2f%~n", [MFA, S, Per])
                end, lists:reverse(lists:keysort(2, OWNs))),
            io:format("~n=== Sorted by ACC:~n"),
            lists:foreach(
                fun({MFA, Per}) ->
                    L = length(MFA),
                    S = lists:duplicate(MaxACC - L + 2, $ ),
                    io:format("~s~s~.2f%~n", [MFA, S, Per])
                end, lists:reverse(lists:keysort(2, ACCs)));
        Err ->
            Err
    end.

mfa_to_list({M, F, A}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
mfa_to_list(F) when is_atom(F) ->
    atom_to_list(F).

find_max(List) ->
    find_max(List, 0).

find_max([{V, _} | Tail], Acc) ->
    find_max(Tail, lists:max([length(V), Acc]));
find_max([], Acc) ->
    Acc.

collect_accs(List) ->
    List1 = lists:filter(
        fun({{sys, _, _}, _, _, _}) ->
            false;
            ({suspend, _, _, _}) ->
                false;
            ({{gen_fsm, _, _}, _, _, _}) ->
                false;
            ({{gen, _, _}, _, _, _}) ->
                false;
            ({{gen_server, _, _}, _, _, _}) ->
                false;
            ({{proc_lib, _, _}, _, _, _}) ->
                false;
            (_) ->
                true
        end, List),
    calculate(List1).

calculate(List1) ->
    TotalACC = lists:sum([A || {_, _, A, _} <- List1]),
    List2 = lists:foldl(fun({MFA, _, ACC, _}, NewList) ->
        Percent = ACC * 100 / TotalACC,
        case round(Percent) of
            0 -> NewList;
            _ -> [{mfa_to_list(MFA), Percent} | NewList]
        end
                        end, [], List1),
    lists:reverse(List2).

all_pids(Type) ->
    lists:foldl(
        fun(P, Acc) when P == self() ->
            Acc;
            (P, Acc) ->
                case catch process_info(
                    P, [message_queue_len, memory, reductions,
                        dictionary, current_function, registered_name]) of
                    [{_, Len}, {_, Memory}, {_, Reds},
                        {_, Dict}, {_, CurFun}, {_, RegName}] ->
                        IntQLen = get_internal_queue_len(Dict),
                        if Type == queue andalso Len == 0 andalso IntQLen == 0 ->
                            Acc;
                            true ->
                                [{lists:max([Len, IntQLen]),
                                    Len, Memory, Reds, Dict, CurFun, P, RegName} | Acc]
                        end;
                    _ ->
                        Acc
                end
        end, [], processes()).

get_internal_queue_len(Dict) ->
    case lists:keysearch('$internal_queue_len', 1, Dict) of
        {value, {_, N}} -> N;
        _ -> 0
    end.

dump(N, Rs) ->
    lists:foreach(
        fun({_, MsgQLen, Memory, Reds, Dict, CurFun, Pid, RegName}) ->
            io:format("** pid(~s)~n"
            "** registered name: ~p~n"
            "** memory: ~p~n"
            "** reductions: ~p~n"
            "** message queue len: ~p~n"
            "** current_function: ~p~n"
            "** dictionary: ~p~n~n",
                [pid_to_list(Pid), RegName, Memory, Reds, MsgQLen, CurFun, Dict])
        end, lists:sublist(Rs, N)).
