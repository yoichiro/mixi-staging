%%
%% @doc This module provides the function to reserve a staging server.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(reserve).
-export([execute/3]).
-include_lib("eunit/include/eunit.hrl").

%%
%% @doc Reserve a staging server.
%% This function will be called from MochiWeb server process.
%%
-spec(execute(pid(), ParameterList, string()) ->
	     ok when
      ParameterList :: [{from, string()} |
			{to, string()} |
			{branch, string()} |
			{category_id, string()} |
			{purpose, string()} |
			tuple(),...]).
execute(Pid, Params, SessionId) ->
    FromDate = list_to_integer(proplists:get_value("from", Params)),
    ToDate = list_to_integer(proplists:get_value("to", Params)),
    Branch = proplists:get_value("branch", Params),
    CategoryId = proplists:get_value("category_id", Params),
    Purpose = proplists:get_value("purpose", Params),
    case db:reserve(FromDate, ToDate, Branch, Purpose, CategoryId, SessionId) of
        {ok, _Booking} ->
            Pid ! {self(),
                   200,
                   {struct, [{result, true}]}};
        available_server_not_found ->
            Pid ! {self(),
                   200,
                   {struct, [{result, false}]}}
    end,
    ok.

%%
%% Tests
%%
-ifdef(TEST).

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
	    meck:expect(db, reserve, fun(_, _, _, _, _, _) ->
						 {ok, true}
					 end),
	    Params = [{"from", "20120309"},
		      {"to", "20120310"},
		      {"branch", "branch1"},
		      {"category_id", "cat1"},
		      {"purpose", "purpose1"}],
	    Pid1 = spawn(reserve,
			 execute,
			 [self(), Params, "session1"]),
	    receive
		Actual1 ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid1, 200,
                                  {struct, [{result, true}]}},
                                 Actual1)
	    end,
	    meck:expect(db, reserve, fun(_, _, _, _, _, _) ->
						 available_server_not_found
					 end),
	    Pid2 = spawn(reserve,
			 execute,
			 [self(), Params, "session1"]),
	    receive
		Actual2 ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid2, 200,
                                  {struct, [{result, false}]}},
                                 Actual2)
	    end
        end)}.

-endif.
