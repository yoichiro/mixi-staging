%%
%% @doc This module provides the function to retrieve categories.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(get_categories).
-export([execute/3]).

-include("data.hrl").

%%
%% @doc Retrieve categories.
%% This function will be called from MochiWeb server process.
%% And, this function expects a second argument does not have any values.
%% The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have multiple category informations.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [tuple(),...]).
execute(Pid, _Params, _SessionId) ->
    Categories = [{struct, [{id, list_to_binary(X#category.id)},
                            {name, list_to_binary(X#category.name)}]}
                  || X <- db:get_categories()],
    Pid ! {self(),
           200,
           {struct, [{result, Categories}]}}.

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
            Categories = [
                          #category{
                             id = "id1",
                             name = "name1"},
                          #category{
                             id = "id2",
                             name = "name2"}],
            meck:expect(db, get_categories, fun() -> Categories end),
            Pid = spawn(?MODULE,
                        execute,
                        [self(), [], "session1"]),
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
