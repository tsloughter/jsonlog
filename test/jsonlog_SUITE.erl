-module(jsonlog_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [{group, jsx}, {group, jsone}].

groups() ->
    [{jsx, [], test_cases()},
     {jsone, [], test_cases()}].

test_cases() ->
    [format].

init_per_group(jsx, Config) ->
    [{encoder, fun(Term, _) -> jsonlog_jsx_encoder:custom_encode(Term) end} | Config];
init_per_group(jsone, Config) ->
    [{encoder, fun jsonlog_jsone_encoder:encode/2} | Config].

end_per_group(_, _) ->
    ok.

format() ->
    [{docs, "Test the formatter json string return value."}].
format(Config) ->
    Encoder = ?config(encoder, Config),

    Map = #{a => b},

    Formatted1 = jsonlog:format(#{level => info, msg => {report, Map}, meta => #{c => d}},
                                #{template => [{body, msg}],
                                  json_encode => Encoder}),
    ?assertEqual(#{<<"body">> => #{<<"a">> => <<"b">>}}, decode(Formatted1)),

    Formatted2 = jsonlog:format(#{level => info, msg => {report, Map}, meta => #{c => [1,2.0,{a,b}]}},
                                #{template => [c, msg],
                                  json_encode => Encoder}),
    ?assertEqual(#{<<"a">> => <<"b">>, <<"c">> => [1, 2.0, <<"{a,b}">>]}, decode(Formatted2)),

    Formatted3 = jsonlog:format(#{level => info, msg => {string, "hello"}, meta => #{c => d}},
                                #{template => [c, msg],
                                  json_encode => Encoder}),
    ?assertEqual(#{<<"body">> => <<"hello">>, <<"c">> => <<"d">>}, decode(Formatted3)),

    Formatted4 = jsonlog:format(#{level => info, msg => {"~p", [{foo}]}, meta => #{c => d}},
                                #{template => [c, msg],
                                  json_encode => Encoder}),
    ?assertEqual(#{<<"body">> => <<"{foo}">>, <<"c">> => <<"d">>}, decode(Formatted4)),

    Map2 = #{e => #{f => g}},

    Formatted5 = jsonlog:format(#{level => info, msg => {report, Map2}, meta => #{c => d}},
                                #{template => [{my_value, msg}],
                                  json_encode => Encoder}),
    ?assertEqual(#{<<"my_value">> => #{<<"e">> => #{<<"f">> => <<"g">>}}}, decode(Formatted5)),

    Map3 = #{e => #{f => #{g => h}}, i => #{j => k}},

    Formatted6 = jsonlog:format(#{level => info, msg => {report, Map3}, meta => #{c => Map3}},
                                #{template => [c],
                                  json_encode => Encoder}),
    ?assertEqual(#{<<"c">> => #{<<"e">> => #{<<"f">> => #{<<"g">> => <<"h">>}}, <<"i">> => #{<<"j">> => <<"k">>}}}, decode(Formatted6)),

    Formatted7 = jsonlog:format(#{level => info, msg => {report, Map3}, meta => #{c => Map3}},
                                #{template => [{c, c}, {msg, msg}],
                                  json_encode => Encoder}),
    ?assertEqual(
        #{
            <<"msg">> => #{<<"e">> => #{<<"f">> => #{<<"g">> => <<"h">>}}, <<"i">> => #{<<"j">> => <<"k">>}},
            <<"c">> => #{<<"e">> => #{<<"f">> => #{<<"g">> => <<"h">>}}, <<"i">> => #{<<"j">> => <<"k">>}}
        }, decode(Formatted7)),

    ok.

%%

decode([Json, _]) ->
    jsone:decode(Json).
