%%
%% @doc This module provides the function to retrieve servers.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(get_servers).
-export([execute/3]).

-include("data.hrl").

%%
%% @doc Retrieve servers.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has an ID of a category
%% as the value of the key named "category_id". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have multiple server informations.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [Category | tuple(),...],
      Category :: {category_id, string()}).
execute(Pid, Params, _SessionId) ->
    CategoryId = proplists:get_value("category_id", Params),
    Servers = [{struct, [{id, list_to_binary(X#server.id)},
                         {name, list_to_binary(X#server.name)}]}
                  || X <- db:get_servers(CategoryId)],
    Pid ! {self(),
           200,
           {struct, [{result, Servers}]}}.

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
	    meck:expect(db, get_servers, fun(_A) ->
						 [#server{
						     id = "id1",
						     name = "name1",
						     category_id = "cat1",
						     deleted = false},
						  #server{
						     id = "id2",
						     name = "name2",
						     category_id = "cat1",
						     deleted = false}]
					 end),
	    Pid = spawn(?MODULE,
			execute,
			[self(),
			 [{"category_id", "cat1"}],
			 "session1"]),
	    receive
		Actual ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid, 200,
                                  {struct, [{result,
                                             [{struct,
                                               [{id, <<"id1">>},
                                                {name, <<"name1">>}]},
                                              {struct,
                                               [{id, <<"id2">>},
                                                {name, <<"name2">>}]}]}]}},
                                 Actual)
	    end
        end)}.

-endif.
