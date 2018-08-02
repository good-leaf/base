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