%%
%% @doc This module has the function to create a new category.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(create_category).
-export([execute/3]).

-include("data.hrl").

%%
%% @doc Create a new category.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has a category name
%% as the value of the key named "name". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have the information about the created category.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [Name | tuple(),...],
      Name :: {name, string()}).
execute(Pid, Params, _SessionId) ->
    CategoryName = proplists:get_value("name", Params),
    {ok, Category} = db:create_category(CategoryName),
    Pid ! {self(),
           200,
           {struct, [{result,
                      {struct, [{id, list_to_binary(Category#category.id)},
                                {name, list_to_binary(Category#category.name)}]}}]}},
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
            meck:expect(db, create_category,
                        fun(CategoryName) ->
                                {ok, #category{id = "id1",
                                               name = CategoryName}}
                        end),
            Pid = spawn(?MODULE,
                        execute,
                        [self(),
                         [{"name", "category1"}],
                         "session1"]),
            receive
                Actual ->
                    ?assert(meck:validate(db)),
                    ?assert(meck:called(db, create_category,
                                        ["category1"])),
                    ?assertEqual({Pid, 200,
                                  {struct, [{result,
                                             {struct,
                                              [{id, <<"id1">>},
                                               {name, <<"category1">>}]}}]}},
                                 Actual)
            end
        end)}.

-endif.
