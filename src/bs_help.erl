-module(bs_help).

%% API
-compile(export_all).

tcpdump_help() ->
    io:format(
        "tcpdump -i eth2 -nn port 5050 -A -s 0 |grep context ~n"
        "tcpdump tcp -i eth1 -t -s 0 -c 100 and dst port ! 22 and src net 192.168.1.0/24 -w ./target.cap ~n"
        "tcpdump tcp -i eth1 -t -s 0 -c 100 and dst port ! 22 and src net 192.168.1.0/24 -w ./target.cap ~n"
        "(1)tcp: ip icmp arp rarp 和 tcp、udp、icmp这些选项等都要放到第一个参数的位置，用来过滤数据报的类型 ~n"
        "(2)-i eth1 : 只抓经过接口eth1的包 ~n"
        "(3)-t : 不显示时间戳 ~n"
        "(4)-s 0 : 抓取数据包时默认抓取长度为68字节。加上-S 0 后可以抓到完整的数据包 ~n"
        "(5)-c 100 : 只抓取100个数据包 ~n"
        "(6)dst port ! 22 : 不抓取目标端口是22的数据包 ~n"
        "(7)src net 192.168.1.0/24 : 数据包的源网络地址为192.168.1.0/24 ~n"
        "(8)-w ./target.cap : 保存成cap文件，方便用ethereal(即wireshark)分析 ~n").

eprof_help() ->
    io:format(
        "eprof:start().~n"
        "eprof:profile([pid(x,x,x)]).~n"
        "eprof:stop_profiling().~n"
        "eprof:analyze().~n"
        "eprof:stop().~n").

property_help() ->
    io:format(
        "~p:memory_sort(N) - show top N pids sorted by memory~n"
        "~p:system() - show system limit~n"
        "~p:cpu_spend() - show cpu spend~n"
        "~p:ets_memory() - show ets memory~n"
        "~p:queue_len() - show queue length~n"
        "~p:queue(N) - show top N pids sorted by queue length~n"
        "~p:memory(N) - show top N pids sorted by memory usage~n"
        "~p:reds(N) - show top N pids sorted by reductions~n"
        "Erlang shell with Ctrl+C~n"
        "~p:eprof_start() - start eprof on all available pids; "
        "DO NOT use on production system!~n"
        "~p:eprof_stop() - stop eprof and print result~n"
        "~p:fprof_start() - start fprof on all available pids; "
        "DO NOT use on production system!~n"
        "~p:fprof_stop() - stop eprof and print formatted result~n"
        "~p:fprof_start(N) - start and run fprof for N seconds; "
        "use ~p:fprof_analyze() to analyze collected statistics and "
        "print formatted result; use on production system with CARE~n"
        "~p:fprof_analyze() - analyze previously collected statistics "
        "using ~p:fprof_start(N) and print formatted result~n"
        "~p:help() - print this help~n",
        lists:duplicate(17, property)).

recon_help() ->
    io:format(
        "recon_trace:calls({Mod, Fun, Args}, Max).~n"
        "recon_trace:calls({Mod, Fun, Args}, Max, []).~n"
        "recon_trace:calls({queue, '_', '_'}, 10).监控queue模块所有函数，10条信息~n"
        "recon_trace:calls({lists, seq, 2}, {100, 1000}).监控lists:seq有2个参数的函数，每秒100条信息~n"
        "recon_trace:calls({lists, seq, fun([_,_,2]) -> ok end}, 100).监控lists:seq(A,B,2)函数~n"
        "recon_trace:calls({erlang, iolist_to_binary, fun([X]) when is_binary(X) -> ok end}, 10).~n"
        "recon_trace:calls({queue, '_', '_'}, {50,1000}, [{pid, Pid}]).每秒50条，进程为Pid的消息~n"
        "recon_trace:calls([{dict,filter,2},{lists,filter,2}], 10, [{pid, new|all|existing}]).监控新创建的进程~n"
        "recon_trace:calls({Mod,Fun,fun(_) -> return_trace() end}, Max, [{args,args}]).显示参数、结果~n"
        "recon_trace:calls({Mod,Fun,[{'_', [], [{return_trace}]}]}, Max, [{args, args}]).显示参数、结果~n"
    ).