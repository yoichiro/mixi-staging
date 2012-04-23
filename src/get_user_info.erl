%%
%% @doc This module provides the function to retrieve an user information.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(get_user_info).
-export([execute/3]).

-include("data.hrl").

%%
%% @doc Retrieve an user information.
%% This function will be called from MochiWeb server process.
%% And, this function retrieves the user information related to the session
%% specified by the third argument value. The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have the user's information.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [tuple(),...]).
execute(Pid, _Params, SessionId) ->
    case db:get_user_by_session_id(SessionId) of
        not_found ->
            Pid ! {self(),
                   200,
                   {struct, [{result, list_to_binary("not_found")}]}};
        User ->
            Pid ! {self(),
                   200,
                   {struct, [{result,
                              {struct,
                               [{id, list_to_binary(User#user.id)},
                                {email, list_to_binary(User#user.email)},
                                {name, list_to_binary(User#user.name)},
                                {irc, list_to_binary(User#user.irc)}
                               ]}}]}}
    end.

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
            meck:expect(db, get_user_by_session_id,
                        fun(_A) -> #user{
                             id = "id1",
                             name = "name1",
                             email = "email1",
                             password = "hashed1",
                             irc = "irc1"}
                        end),
            Pid1 = spawn(?MODULE,
                         execute,
                         [self(),
                          [],
                          "session1"]),
            receive
                Actual1 ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid1, 200,
                                  {struct, [{result,
                                             {struct,
                                              [{id, <<"id1">>},
                                               {email, <<"email1">>},
                                               {name, <<"name1">>},
                                               {irc, <<"irc1">>}]}}]}},
                                 Actual1)
            end,
            meck:expect(db, get_user_by_session_id,
                        fun(_A) -> not_found end),
            Pid2 = spawn(?MODULE,
                         execute,
                         [self(),
                          [],
                          "session1"]),
            receive
                Actual2 ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid2, 200,
                                  {struct, [{result, <<"not_found">>}]}},
                                 Actual2)
            end
        end)}.

-endif.
