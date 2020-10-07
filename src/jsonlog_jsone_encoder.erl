-module(jsonlog_jsone_encoder).

-export([encode/2]).

encode(Log, _Config) ->
    jsone:encode(Log, [native_utf8,
                       native_forward_slash,
                       {float_format, [{decimals, 4}, compact]},
                       {map_unknown_value, fun(Term) ->
                                                   {ok, jsonlog_utils:to_string(Term)}
                                           end}]).
