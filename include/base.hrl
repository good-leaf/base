%log
-define(HLOG(HashKey, FileName, Format, Message), bs_log:hash_log(HashKey, FileName, Format, Message)).

-define(DEBUG(Format), lager:debug(Format)).
-define(INFO(Format), lager:info(Format)).
-define(WARNING(Format), lager:warning(Format)).
-define(ERROR(Format), lager:error(Format)).

-define(DEBUG(Format, Message), lager:debug(Format, Message)).
-define(INFO(Format, Message), lager:info(Format, Message)).
-define(WARNING(Format, Message), lager:warning(Format, Message)).
-define(ERROR(Format, Message), lager:error(Format, Message)).


-define(MILLISECONDS_IN_SECOND, 1000).
-define(WORKER(I), {I, {I, 'start_link', []}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_ARGS(I, Args), {I, {I, 'start_link', Args}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_TYPE(I, Type), {I, {I, 'start_link', []}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_ARGS_TYPE(I, Args, Type), {I, {I, 'start_link', Args}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_NAME_ARGS(I, N, Args), {N, {I, 'start_link', Args}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_NAME_ARGS_TYPE(N, I, Args, Type), {N, {I, 'start_link', Args}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_APP_INIT(I, T), {I, {I, 'start_link', []}, 'temporary', T * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).

-define(SUPER(I), {I, {I, 'start_link', []}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_TYPE(I, Type), {I, {I, 'start_link', []}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_ARGS(I, Args), {I, {I, 'start_link', Args}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_ARGS_TYPE(I, Args, Type), {I, {I, 'start_link', Args}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_ARGS(I, N, Args), {N, {I, 'start_link', Args}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_ARGS_TYPE(N, I, Args, Type), {N, {I, 'start_link', Args}, Type, 'infinity', 'supervisor', [I]}).
