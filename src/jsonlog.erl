-module(jsonlog).

-export([format/2]).

-type template() :: [metakey() | {metakey(), template(), template()} | string()].
-type metakey() :: atom() | [atom()].

%%====================================================================
%% API functions
%%====================================================================
-spec format(LogEvent, Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: logger:formatter_config().
format(Map = #{msg := {report, #{label := {error_logger, _}, format := Format, args := Terms}}}, UsrConfig) ->
    format(Map#{msg := {report,
                        #{body =>
                              unicode:characters_to_binary(io_lib:format(Format, Terms))}}},
           UsrConfig);
format(#{level := Level, msg := {report, Msg}, meta := Meta}, UserConfig) when is_map(Msg) ->
    Config = apply_defaults(UserConfig),
    NewMeta = maps:merge(Meta, #{level => Level}),
    format_log(maps:get(template, Config), Config, Msg, NewMeta);
format(Map = #{msg := {string, String}}, UsrConfig) ->
    format(Map#{msg := {report,
                        #{body =>
                              unicode:characters_to_binary(String)}}}, UsrConfig);
format(Map = #{msg := {Format, Terms}}, UserConfig) ->
    format(Map#{msg := {report,
                        #{body =>
                              unicode:characters_to_binary(io_lib:format(Format, Terms))}}},
           UserConfig).

-spec format_log(template(), Config, Msg, Meta) -> unicode:chardata() when
      Config :: logger:formatter_config(),
      Msg :: Data,
      Meta :: Data,
      Data :: #{string() | binary() | atom() => term()}.
format_log(Tpl, Config=#{json_encode := JsonEncode}, Msg, Meta) ->
    %% need `native_forward_slash' to not get like `mod:fun\/1' for mfa
    [JsonEncode(format_log(Tpl, Config, Msg, Meta, #{}), Config), "\n"].

format_log([], _Config, _Msg, _Meta, Acc) ->
    Acc;
format_log([{JsonKey, msg} | Rest], Config, Msg, Meta, Acc) ->
    format_log(Rest, Config, Msg, Meta, Acc#{JsonKey => format_msg(Msg, Config)});
format_log([msg | Rest], Config, Msg, Meta, Acc) ->
    format_log(Rest, Config, Msg, Meta, maps:merge(Acc, format_msg(Msg, Config)));
format_log([{JsonKey, TemplateKey} | Rest], Config, Msg, Meta, Acc) when is_atom(TemplateKey) ->
    case maps:find(TemplateKey, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, Acc);
        {ok, Val} ->
            format_log(Rest, Config, Msg, Meta, Acc#{JsonKey => format_val(TemplateKey, Val, Config)})
    end;
format_log([TemplateKey | Rest], Config, Msg, Meta, Acc) ->
    case maps:find(TemplateKey, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, Acc);
        {ok, Val} ->
            format_log(Rest, Config, Msg, Meta, Acc#{TemplateKey => format_val(TemplateKey, Val, Config)})
    end.

%%

apply_defaults(Map) ->
    maps:merge(
      #{time_offset => 0,
        time_designator => $T,
        template => [{time, time},
                     {level, level},
                     {pid, pid},
                     {in, mfa},
                     {line, line},
                     msg]},
      Map).

format_msg(Data, Config) ->
    format_msg("", Data, Config).

format_msg(_Parents, Data, _Config) ->
    Data.

format_val(time, Time, Config) ->
    format_time(Time, Config);
format_val(mfa, MFA, Config) ->
    format_mfa(MFA, Config);
format_val(_Key, Val, _Config) ->
    jsonlog_utils:to_string(Val).

format_time(N, #{time_offset := O, time_designator := D}) when is_integer(N) ->
    list_to_binary(calendar:system_time_to_rfc3339(N, [{unit, microsecond},
                                                       {offset, O},
                                                       {time_designator, D}])).

format_mfa({M, F, A}, _) when is_atom(M), is_atom(F), is_integer(A) ->
   <<(atom_to_binary(M, utf8))/binary, $:, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>;
format_mfa({M, F, A}, Config) when is_atom(M), is_atom(F), is_list(A) ->
    %% arguments are passed as a literal list ({mod, fun, [a, b, c]})
    format_mfa({M, F, length(A)}, Config);
format_mfa(MFAStr, _Config) -> % passing in a pre-formatted string value
    jsonlog_utils:to_string(MFAStr).
