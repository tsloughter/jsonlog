-module(jsonlog_utils).

-export([to_string/1]).

to_string(T) when is_atom(T) ->
    T;
to_string(T) when is_integer(T) ->
    T;
to_string(T) when is_float(T) ->
    T;
to_string(T) when is_pid(T) ->
    erlang:list_to_binary(erlang:pid_to_list(T));
to_string(T) when is_reference(T) ->
    erlang:list_to_binary(erlang:ref_to_list(T));
to_string(T) when is_binary(T) ->
    T;
to_string(T) when is_list(T) ->
    case io_lib:printable_list(T) of
        true ->
            case unicode:characters_to_binary(T) of
                {_, _, _} -> % error or incomplete
                    list_to_binary(io_lib:format("~tp", [T]));
                Binary ->
                    Binary
            end;
        false ->
            [to_string(E) || E <- T]
    end;
to_string(T) ->
    %% TODO: figure out how we can do term_depth with jsx
    %% currently can't pass a depth value in a config to
    %% jsx config so that feature is not available
    list_to_binary(io_lib:format("~tp", [T])).
