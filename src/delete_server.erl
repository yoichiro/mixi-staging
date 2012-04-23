%%
%% @doc This module has the function to delete a server.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(delete_server).
-export([execute/3]).

%%
%% @doc Delete a server.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has an ID of a server
%% as the value of the key named "server_id". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have the flag, and its value is always true.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [Server | tuple(),...],
      Server :: {server_id, string()}).
execute(Pid, Params, _SessionId) ->
    ServerId = proplists:get_value("server_id", Params),
    db:delete_server(ServerId),
    Pid ! {self(),
           200,
           {struct, [{result, true}]}},
    ok.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

meck_setup() ->
    Modules = [db],
    meck:new(Modules),
    Modules.

execute_test_() ->
    {setup,
     fun meck_setup/0,
     fun meck:unload/1,
     ?_test(
        begin
            meck:expect(db, delete_server,
                        fun(_ServerId) ->
                                ok
                        end),
            Pid = spawn(?MODULE,
                        execute,
                        [self(),
                         [{"server_id", "id1"}],
                         "session1"]),
            receive
                Actual ->
                    ?assert(meck:validate(db)),
                    ?assert(meck:called(db, delete_server,
                                        ["id1"])),
                    ?assertEqual({Pid, 200,
                                  {struct, [{result, true}]}},
                                 Actual)
            end
        end)}.

-endif.
