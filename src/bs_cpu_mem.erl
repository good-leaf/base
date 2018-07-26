-module(bs_cpu_mem).

%% API
-compile(export_all).

%每10s更新一次
etop_cpu() ->
    spawn(fun() -> etop:start([{interval, 10}, {sort, runtime}]) end).

%输出20个进程
etop_memory() ->
    spawn(fun() -> etop:start([{output, text}, {lines, 20}, {sort, memory}]) end).

etop_memory(PN) ->
    spawn(fun() -> etop:start([{output, text}, {lines, PN}, {sort, memory}]) end).

etop_stop() ->
    etop:stop().