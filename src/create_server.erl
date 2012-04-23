%%
%% @doc This module has the function to create a new server.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(create_server).
-export([execute/3]).

-include("data.hrl").

%%
%% @doc Create a new server belonging to the specified category.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has a server name
%% as the value of the key named "name" and a category ID as the value of
%% the key named "category_id". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have the information about the created server.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [Category | Name | tuple(),...],
      Category :: {category_id, string()},
      Name :: {name, string()}).
execute(Pid, Params, _SessionId) ->
    CategoryId = proplists:get_value("category_id", Params),
    ServerName = proplists:get_value("name", Params),
    {ok, Server} = db:create_server(CategoryId, ServerName),
    Pid ! {self(),
           200,
           {struct, [{result,
                      {struct, [{id, list_to_binary(Server#server.id)},
                                {name, list_to_binary(Server#server.name)}]}}]}},
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
            meck:expect(db, create_server,
                        fun(_CategoryId, ServerName) ->
                                {ok, #server{id = "id1",
                                             name = ServerName}}
                        end),
            Pid = spawn(?MODULE,
                        execute,
                        [self(),
                         [{"category_id", "id1"},
                          {"name", "server1"}],
                         "session1"]),
            receive
                Actual ->
                    ?assert(meck:validate(db)),
                    ?assert(meck:called(db, create_server,
                                        ["id1", "server1"])),
                    ?assertEqual({Pid, 200,
                                  {struct, [{result,
                                             {struct,
                                              [{id, <<"id1">>},
                                               {name, <<"server1">>}]}}]}},
                                 Actual)
            end
        end)}.

-endif.
