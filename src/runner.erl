-module(runner).

-export([spawn/1,

         exec/2]).

-record(runner, {owner, port, opts}).
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

spawn(Opts) ->
    Dir = proplists:get_value(cd, Opts, element(2, file:get_cwd())),
    Env = proplists:get_value(env, Opts, []),

    ActualCmd = "/bin/sh -s unix:cmd",
    Owner = self(),
    {Fun, Acc} = case proplists:get_value(fold, Opts) of
                     {_,_} = X -> X;
                     undefined ->
                         {fun(X, A) ->
                                  stream_to(Owner, X, A)
                          end,
                          []}
                 end,
    
    spawn_link(fun() ->
                       Port = open_port({spawn, ActualCmd}, [{cd, Dir}, {env, Env},
                                                             exit_status, {line, 16384}, binary,
                                                             use_stdio, stderr_to_stdout]),
                       loop(#runner{port = Port, owner = Owner, opts = Opts}, Fun, Acc)
               end).

stream_to(Owner, eoc, A) ->
    Owner ! {eoc, self(), lists:reverse(A)},
    [];
stream_to(_Owner, {line, L}, A) ->
    [L | A];
stream_to(_Owner, {more, L}, A) ->
    [L | A];
stream_to(_Owner, done, Acc) ->
    [] = Acc,
    ok;
stream_to(_Owner, {error, Rc}, A) ->
    error({Rc, lists:reverse(A)}).

exec(Pid, Cmd) ->
    Pid ! {cmd, Cmd}.
    
loop(#runner{port = Port} = R, Fun, Acc) ->
    receive
        {cmd, Cmd} ->
            port_command(Port, mk_cmd(Cmd)),
            loop(R, Fun, Acc);
        {Port, {data, {eol, <<4,4,4,4>>}}} ->
            loop(R, Fun, Fun(eoc, Acc));            
        {Port, {data, {eol, Line}}} ->
            loop(R, Fun, Fun({line, <<Line/binary, "\n">>}, Acc));
        {Port, {data, {noeol, Line}}} ->
            loop(R, Fun, Fun({more, Line}, Acc));
        {Port, {exit_status, 0}} ->
            Fun(done, Acc);
        {Port, {exit_status, Rc}} ->
            Fun({error, Rc}, Acc)
    end.
    
mk_cmd(Cmd) ->
    io_lib:format("~s;\n echo  \"\^D\^D\^D\^D\"\n", [Cmd]).
    
