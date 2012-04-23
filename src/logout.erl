%%
%% @doc This module provides the function to logged out.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(logout).
-export([execute/3]).

%%
%% @doc Logged out from a specified session.
%% This function will be called from MochiWeb server process.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [tuple(),...]).
execute(Pid, _Params, SessionId) ->
    db:delete_session(SessionId),
    Pid ! {self(),
           201,
           {struct, [{result, <<"deleted">>}]}}.

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
	    meck:expect(db, delete_session, fun(_A) -> ok end),
	    Pid1 = spawn(?MODULE,
			 execute,
			 [self(),
			  [],
			  "session1"]),
	    receive
		Actual ->
                    ?assert(meck:validate(db)),
		    ?assert(meck:called(db, delete_session, ["session1"])),
                    ?assertEqual({Pid1, 201,
                                  {struct, [{result, <<"deleted">>}]}},
                                 Actual)
	    end
        end)}.

-endif.
