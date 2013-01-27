-module(runner).

-export([spawn/1,

         exec/2, exec/3,

         exec_stream_sync/4, exec_stream_sync/5,

         exec_aggregate/2, exec_aggregate/3,
         exec_aggregate_sync/2, exec_aggregate_sync/3]).

-record(runner, {owner, port, opts}).
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

spawn(Opts) ->
    Dir = proplists:get_value(cd, Opts, element(2, file:get_cwd())),
    Env = proplists:get_value(env, Opts, []),
    ExitOnError = proplists:get_value(exit_on_error, Opts, false),
    Prefix = proplists:get_value(prefix, Opts, ""),

    ActualCmd = string:strip(Prefix ++ " /bin/sh -s unix:runner"),
    Owner = self(),
    {Fun, Acc} = case proplists:get_value(fold, Opts) of
                     {_,_} = X -> X;
                     undefined ->
                         {fun(_, _) -> ok end, []}
                 end,

    Spawn = case proplists:get_value(link, Opts, true) of
                true -> fun erlang:spawn_link/1;
                false -> fun erlang:spawn/1
            end,

    Spawn(fun() ->
                  Port = open_port({spawn, ActualCmd}, [{cd, Dir}, {env, Env},
                                                        exit_status, {line, 16384}, binary,
                                                        use_stdio, stderr_to_stdout]),
                  link(Port),
                  case ExitOnError of
                      true ->
                          port_command(Port, "set -e;\n");
                      _ -> ok
                  end,
                  loop(#runner{port = Port, owner = Owner, opts = Opts}, Fun, Acc)
          end).

exec(Pid, Cmd) ->
    Pid ! {cmd, Cmd}.
exec(Pid, Cmd, Fold) when is_function(Fold) ->
    Pid ! {cmd, Cmd, {Fold, []}};
exec(Pid, Cmd, {F, A}) ->
    Pid ! {cmd, Cmd, {F, A}}.

exec_aggregate_sync(Pid, Cmd, Opts) ->
    case proplists:get_value(cd, Opts) of
        undefined -> ok;
        Dir ->
            exec_aggregate_sync(Pid, ["cd ", Dir])
    end,
    exec_aggregate_sync(Pid, Cmd).
exec_aggregate_sync(Pid, Cmd) ->
    exec_aggregate(Pid, Cmd, self()),
    receive
        {eoc, Pid, 0, R} ->
            {ok, R};
        {eoc, Pid, Error, R} ->
            {error, {Error, R}}
    end.

exec_aggregate(Pid, Cmd) ->
    exec_aggregate(Pid, Cmd, self()).
exec_aggregate(Pid, Cmd, Owner) ->
    Fold = fun(X, A) ->
                   cmd_aggregate(Owner, X, A)
           end,
    exec(Pid, Cmd, Fold).

exec_stream_sync(Pid, Cmd, Ref, Parent, Opts) ->
    case proplists:get_value(cd, Opts) of
        undefined -> ok;
        Dir -> exec_aggregate_sync(Pid, ["cd ", Dir])
    end,
    exec_stream_sync(Pid, Cmd, Ref, Parent).

exec_stream_sync(Pid, Cmd, Ref, Parent) ->
    Owner = self(),
    Send = fun(X, A) ->
                   cmd_stream_to(Owner, Ref, Parent, X, A)
           end,
    exec(Pid, Cmd, Send),
    receive
        {eoc, Pid, 0, R} ->
            {ok, R};
        {eoc, Pid, Error, R} ->
            {error, {Error, R}}
    end.





cmd_aggregate(Owner, X, A) ->
    case X of
        {eoc, Error} ->
            Owner ! {eoc, self(), Error, lists:reverse(A)},
            [];
        {line, L} ->
            [L | A];
        {more, L} ->
            [L | A];
        done ->
            [];
        {error, Rc} ->
            error({Rc, lists:reverse(A)})
    end.


cmd_stream_to(Owner, Ref, Parent, X, A) ->
    case X of
        {eoc, Error} ->
            Owner ! {eoc, self(), Error, lists:reverse(A)}, [];
        {line, L} ->
            Parent ! {Ref, line, L}, [];
        {more, L} ->
            Parent ! {Ref, line, L}, [];
        done ->
            A;
        {error, Rc} ->
            error({Rc, lists:reverse(A)})
    end.


loop(#runner{port = Port} = R, Fun, Acc) ->
    receive
        {cmd, Cmd} ->
            port_command(Port, mk_cmd(Cmd)),
            loop(R, Fun, Acc);
        {cmd, Cmd, {F, A}} ->
            port_command(Port, mk_cmd(Cmd)),
            loop(R, F, A);
        {Port, {data, {eol, <<4,4,4,4, Error/binary>>}}} ->
            loop(R, Fun, rep(Fun, {eoc, list_to_integer(binary_to_list(Error))}, Acc));
        {Port, {data, {eol, Line}}} ->
            loop(R, Fun, rep(Fun, {line, <<Line/binary, "\n">>}, Acc));
        {Port, {data, {noeol, Line}}} ->
            loop(R, Fun, rep(Fun, {more, Line}, Acc));
        {Port, {exit_status, 0}} ->
            rep(Fun, done, Acc);
        {Port, {exit_status, Rc}} ->
            rep(Fun, {error, Rc}, Acc)
    end.

rep(Fun, X, A) ->
    Fun(X, A).

mk_cmd(Cmd) ->
    binary_to_list(iolist_to_binary(io_lib:format("~s;\n echo  \"\^D\^D\^D\^D$?\"\n", [Cmd]))).
