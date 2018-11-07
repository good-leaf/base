-module(ba_auth).

%% API
-export([
    get_date_gmt/0,
    f/2,
    get_ba_header/4,
    get_auth/5
]).

-spec(get_auth(Date :: string(), Method :: string(), Path :: string(),
    ClientId::string(), ClientSecret::string() ) -> string()).
get_auth(Date, Method, Path, ClientId, ClientSecret) ->
    Sign = base64:encode(crypto:hmac(sha, ClientSecret, Method ++ " " ++ Path ++ "\n" ++ Date)),
    "MWS " ++ ClientId ++ ":" ++ binary_to_list(Sign).


get_ba_header(Method, Path, ClientId, ClientSecret) ->
    Date = get_date_gmt(),
    Auth = get_auth(Date, Method, Path, ClientId, ClientSecret),
    [
        {"Date", Date},
        {"Authorization", Auth}
    ].

-spec(get_date_gmt() -> string()).
get_date_gmt() ->
    f(os:timestamp(), "%a, %d %b %Y %H:%M:%S GMT", universal).

-spec(f(Now :: tuple(), FormatStr :: atom()) -> string()).
f(Now, FormatStr) ->
    f(Now, FormatStr, local).

f({_MegaSec, _Sec, _MicroSec} = Now, FormatStr, ZONAL) when is_list(FormatStr) ->
    ZONALF = case ZONAL of
                 local -> now_to_local_time;
                 universal -> now_to_universal_time;
                 Else -> Else
             end,
    Res = [do_f(Now, FPart, ZONALF) || FPart <- re:split(FormatStr, "([%][^%])")],
    binary_to_list(list_to_binary(Res)).

do_f(Tm, <<"%d">>, ZONAL) ->
    {{_YY, _MM, DD}, _} = calendar:ZONAL(Tm),
    f2(DD);

do_f(Tm, <<"%e">>, ZONAL) ->
    {{_YY, _MM, DD}, _} = calendar:ZONAL(Tm),
    pad(integer_to_list(DD), 2);

do_f(Tm, <<"%m">>, ZONAL) ->
    {{_YY, MM, _DD}, _} = calendar:ZONAL(Tm),
    f2(MM);

do_f(Tm, <<"%y">>, ZONAL) ->
    {{YY, _MM, _DD}, _} = calendar:ZONAL(Tm),
    f2(YY);

do_f(Tm, <<"%Y">>, ZONAL) ->
    {{YY, _MM, _DD}, _} = calendar:ZONAL(Tm),
    f4(YY);

do_f(Tm, <<"%C">>, ZONAL) ->
    {{YY, _MM, _DD}, _} = calendar:ZONAL(Tm),
    f2(round(YY / 100));

do_f(Tm, <<"%H">>, ZONAL) ->
    {_, {H, _M, _S}} = calendar:ZONAL(Tm),
    f2(H);

do_f(Tm, <<"%l">>, ZONAL) ->
    {_, {H, _M, _S}} = calendar:ZONAL(Tm),
    pad(integer_to_list(H), 2);

do_f(Tm, <<"%k">>, ZONAL) ->
    {_, {H, _M, _S}} = calendar:ZONAL(Tm),
    pad(integer_to_list(H), 2);

do_f(Tm, <<"%I">>, ZONAL) ->
    {_, {H, _M, _S}} = calendar:ZONAL(Tm),
    case H < 13 of
        true -> f2(H);
        false -> f2(H - 12)
    end;

do_f(Tm, <<"%M">>, ZONAL) ->
    {_, {_H, M, _S}} = calendar:ZONAL(Tm),
    f2(M);

do_f(Tm, <<"%S">>, ZONAL) ->
    {_, {_H, _M, S}} = calendar:ZONAL(Tm),
    f2(S);

do_f(Tm, <<"%u">>, ZONAL) ->
    {Date, _} = calendar:ZONAL(Tm),
    integer_to_list(calendar:day_of_the_week(Date));

do_f(Tm, <<"%w">>, ZONAL) ->
    {Date, _} = calendar:ZONAL(Tm),
    Day = calendar:day_of_the_week(Date),
    WDay = case Day of
               7 -> 0;
               _ -> Day - 1
           end,
    integer_to_list(WDay);

do_f({MegaSec, Sec, _}, <<"%s">>, _) ->
    integer_to_list(1000000 * MegaSec + Sec);

do_f(Tm, <<"%b">>, Z) -> abrv_mon(lists:flatten(do_f(Tm, <<"%m">>, Z)));
do_f(Tm, <<"%h">>, Z) -> do_f(Tm, <<"%b">>, Z);
do_f(Tm, <<"%B">>, Z) -> month(lists:flatten(do_f(Tm, <<"%m">>, Z)));
do_f(Tm, <<"%a">>, Z) -> abrv_day(lists:flatten(do_f(Tm, <<"%u">>, Z)));
do_f(Tm, <<"%A">>, Z) -> weekday(lists:flatten(do_f(Tm, <<"%u">>, Z)));

do_f(Tm, <<"%p">>, ZONAL) ->
    {_, {H, _M, _S}} = calendar:ZONAL(Tm),
    case H < 12 of
        true -> "AM";
        false -> "PM"
    end;

do_f(Tm, <<"%P">>, ZONAL) ->
    {_, {H, _M, _S}} = calendar:ZONAL(Tm),
    case H < 12 of
        true -> "am";
        false -> "pm"
    end;

do_f({_, _, MicroSec}, <<"%N">>, _) -> integer_to_list(MicroSec);
do_f({_, _, MicroSec}, <<"%L">>, _) -> f3(round(MicroSec / 1000));

do_f(Tm, <<"%D">>, Z) -> f(Tm, "%m/%d/%y", Z);
do_f(Tm, <<"%F">>, Z) -> f(Tm, "%Y-%m-%d", Z);
do_f(Tm, <<"%T">>, Z) -> f(Tm, "%H:%M:%S", Z);
do_f(Tm, <<"%R">>, Z) -> f(Tm, "%H:%M", Z);
do_f(Tm, <<"%r">>, Z) -> f(Tm, "%I:%M:%S %p", Z);
do_f(Tm, <<"%v">>, Z) -> f(Tm, "%e-%b-%Y", Z);

do_f(_Tm, Str, _) -> Str.

f2(N) -> io_lib:format("~2.2.0w", [(N rem 100)]).
f3(N) -> io_lib:format("~3.3.0w", [(N rem 1000)]).
f4(N) -> io_lib:format("~4.4.0w", [(N rem 10000)]).

pad(Str, 2) when length(Str) < 2 -> [" ", Str];
pad(Str, _N) -> Str.

abrv_day("1") -> "Mon";
abrv_day("2") -> "Tue";
abrv_day("3") -> "Wed";
abrv_day("4") -> "Thu";
abrv_day("5") -> "Fri";
abrv_day("6") -> "Sat";
abrv_day("7") -> "Sun".

weekday("1") -> "Monday";
weekday("2") -> "Tuesday";
weekday("3") -> "Wednesday";
weekday("4") -> "Thursday";
weekday("5") -> "Friday";
weekday("6") -> "Saturday";
weekday("7") -> "Sunday".

abrv_mon("01") -> "Jan";
abrv_mon("02") -> "Feb";
abrv_mon("03") -> "Mar";
abrv_mon("04") -> "Apr";
abrv_mon("05") -> "May";
abrv_mon("06") -> "Jun";
abrv_mon("07") -> "Jul";
abrv_mon("08") -> "Aug";
abrv_mon("09") -> "Sep";
abrv_mon("10") -> "Oct";
abrv_mon("11") -> "Nov";
abrv_mon("12") -> "Dec".

month("01") -> "January";
month("02") -> "February";
month("03") -> "March";
month("04") -> "April";
month("05") -> "May";
month("06") -> "June";
month("07") -> "July";
month("08") -> "August";
month("09") -> "September";
month("10") -> "October";
month("11") -> "November";
month("12") -> "December".
