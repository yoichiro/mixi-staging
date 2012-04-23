%%
%% @doc This module has the function to delete a category.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(delete_category).
-export([execute/3]).

%%
%% @doc Delete a category.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has an ID of a category
%% as the value of the key named "category_id". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have the flag, and its value is always true.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [tuple(),...]).
execute(Pid, Params, _SessionId) ->
    CategoryId = proplists:get_value("category_id", Params),
    db:delete_category(CategoryId),
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
            meck:expect(db, delete_category,
                        fun(_CategoryId) ->
                                ok
                        end),
            Pid = spawn(?MODULE,
                        execute,
                        [self(),
                         [{"category_id", "id1"}],
                         "session1"]),
            receive
                Actual ->
                    ?assert(meck:validate(db)),
                    ?assert(meck:called(db, delete_category,
                                        ["id1"])),
                    ?assertEqual({Pid, 200,
                                  {struct, [{result, true}]}},
                                 Actual)
            end
        end)}.

-endif.
