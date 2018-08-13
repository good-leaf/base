%log
-define(HLOG(HashKey, FileName, Format, Message), bs_log:hash_log(HashKey, FileName, Format, Message)).

-define(LDEBUG(Format), lager:debug(Format)).
-define(LINFO(Format), lager:info(Format)).
-define(LWARNING(Format), lager:warning(Format)).
-define(LERROR(Format), lager:error(Format)).

-define(LDEBUG(Format, Message), lager:debug(Format, Message)).
-define(LINFO(Format, Message), lager:info(Format, Message)).
-define(LWARNING(Format, Message), lager:warning(Format, Message)).
-define(LERROR(Format, Message), lager:error(Format, Message)).

-define(DEBUG(Format), sdebug:debug(bs_log:format("debug", Format))).
-define(INFO(Format), sinfo:info(bs_log:format("info", Format))).
-define(WARNING(Format), swarning:warning(bs_log:format("warning", Format))).
-define(ERROR(Format), serror:error(bs_log:format("error", Format))).

-define(DEBUG(Format, Message), sdebug:debug(bs_log:format("debug", Format), Message)).
-define(INFO(Format, Message), sinfo:info(bs_log:format("info", Format), Message)).
-define(WARNING(Format, Message), swarning:warning(bs_log:format("warning", Format), Message)).
-define(ERROR(Format, Message), serror:error(bs_log:format("error", Format), Message)).
