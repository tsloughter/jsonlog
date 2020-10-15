jsonlog
=====

![Build Status](https://github.com/tsloughter/jsonlog/workflows/Common%20Test/badge.svg)

Formatter for the [Erlang/OTP logger](https://erlang.org/doc/apps/kernel/logger_chapter.html) that outputs json.

Based on [flatlog](https://github.com/ferd/flatlog/)

## Usage

``` erlang
[
 {kernel, [
    {logger, [
        {handler, default, logger_std_h,
         #{formatter => {jsonlog, #{
            json_encode => fun jsonlog_jsone_encoder:encode/2
          }}}
        }
    ]},
    {logger_level, info}
 ]}
].
```

Any function that returns a string can be used for the value of `json_encode`,
but `jsonlog` comes with implementations for
[jsone](https://github.com/sile/jsone) and
[jsx](https://github.com/talentdeficit/jsx/).

The formatter takes a template that is used to construct the json term. The
template is a list of 2-tuples where the first element is the key name that will
be used in the json object and the second element is the key to lookup in the
metadata. If the key does not exist in the metadata then the key is left out of
the result.

Examples using the default template:

``` erlang
> logger:info("hello").
{"body":{"body":"hello"},"level":"info","pid":"<0.155.0>","time":"2020-10-10T16:55:14.346881+00:00"}
> logger:info(#{structured => <<"msg">>}).
{"body":{"structured":"msg"},"level":"info","pid":"<0.155.0>","time":"2020-10-10T16:56:01.573335+00:00"}
```

Note that an Erlang string as a list of characters will be a list of integers
with the default jsx and jsone encoders at this time. But this isn't an issue
with a plain string message like `"hello"` above.

``` erlang
> logger:info(#{a => "hello"}).
{"body":{"a":[104,101,108,108,111]},"level":"info","pid":"<0.155.0>","time":"2020-10-10T17:01:05.201554+00:00"}
```

The `msg` template key is allowed to be bare, not in a 2-tuple, in which case it
will merge the log message with the rest of the json object instead of putting
it under a separate key. So if the template was set to `[{time, time}, {level,
level}, {pid, pid}, msg]}` the resulting json object would look like:

``` erlang
logger:info(#{a => <<"hello">>}).
{"a":"hello","level":"info","pid":"<0.155.0>","time":"2020-10-10T17:05:41.471383+00:00"}
```

## Use from Elixir

Configure your application with a logger config like the one for Erlang. Based
on [Elixir Logger docs for using Erlang/OTP handlers](https://hexdocs.pm/logger/Logger.html#module-erlang-otp-handlers):

``` elixir
import Config

config :logger,
  backends: []
  
config :your_app, :logger, [
  {:handler, :default, :logger_std_h,
   %{formatter: {:jsonlog, %{json_encode: &:jsonlog_jsone_encoder.encode/2}}}}]
```

Then you have to manually call `:logger.add_handlers` in your Application code
when it starts up.

``` elixir
$ iex -S mix
> :logger.add_handlers(:your_app)
:ok
> :logger.info(%{a: "b"})
{"body": {"a":"b"},"level":"info","pid":"<0.174.0>","time":"2020-10-15T22:52:29.598551+00:00"}
```

## Todo

- Open question: Should the log message map be able to be used like metadata
  where keys in the template check it for values?
- `term_depth`: Can we implement something like that? `jsx` doesn't have the
  ability to pass a configuration through to the encoder
