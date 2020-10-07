-module(jsonlog_jsx_encoder).

-export([custom_encode/1]).
%% custom encoder callback
-export([encode/2]).

%% the parser is unmodified but the term passed to the parser instance is
%% preencoded by the custom `encode/2` function below
custom_encode(Term) ->
    (jsx:parser(jsx_to_json, [], [escaped_strings]))(encode(Term, ?MODULE) ++ [end_json]).

%% this captures any instance that matches the record as a top level term, the value
%% for a map or proplist entry or a member of a list and returns a map to the parser
%% instead. anything else is handled by the default parser in `jsx_encoder`
encode(Pid, _EntryPoint) when is_pid(Pid) ->
    [jsonlog_utils:to_string(Pid)];
encode(Ref, _EntryPoint) when is_reference(Ref) ->
    [jsonlog_utils:to_string(Ref)];
encode(Tuple, _EntryPoint) when erlang:is_tuple(Tuple) , erlang:tuple_size(Tuple) > 2 ->
    %% should tuples be converted to lists or just turned into strings
    [jsonlog_utils:to_string(Tuple)];
encode(Term, EntryPoint) ->
    jsx_encoder:encode(Term, EntryPoint).
