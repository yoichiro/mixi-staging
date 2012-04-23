%%
%% @doc This module provides to register a new user.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(register_new_user).
-export([execute/3]).

%%
%% @doc Regsiter a new user.
%% This function will be called from MochiWeb server process.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [{email, string()} |
                        {name, string()} |
                        {password, string()} |
                        {irc, string()} |
                        tuple(),...]).
execute(Pid, Params, _SessionId) ->
    Email = proplists:get_value("email", Params),
    Password = proplists:get_value("password", Params),
    Name = proplists:get_value("name", Params),
    Irc = proplists:get_value("irc", Params),
    case db:create_user(Name, Email, Password, Irc) of
        {ok, _NewUser} ->
            Pid ! {self(),
                   200,
                   {struct, [{result, true}]}};
        {already_exists, _User} ->
            Pid ! {self(),
                   200,
                   {struct, [{result, false}, {reason, "already_exists"}]}}
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
            meck:expect(db, create_user, fun(_Name, _Email, _Password, _Irc) ->
                                                 {ok, true}
                                         end),
            Pid1 = spawn(?MODULE,
                         execute,
                         [self(),
                          [{"email", "email1"},
                           {"password", "password1"},
                           {"name", "name1"},
                           {"irc", "irc1"}],
                          "session1"]),
            receive
                Actual1 ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid1, 200,
                                  {struct, [{result, true}]}},
                                 Actual1)
            end,
            meck:expect(db, create_user, fun(_Name, _Email, _Password, _Irc) ->
                                                 {already_exists, false}
                                         end),
            Pid2 = spawn(?MODULE,
                         execute,
                         [self(),
                          [{"email", "email1"},
                           {"password", "password1"},
                           {"name", "name1"},
                           {"irc", "irc1"}],
                          "session1"]),
            receive
                Actual2 ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid2, 200,
                                  {struct, [{result, false},
                                            {reason, "already_exists"}]}},
                                 Actual2)
            end
        end)}.

-endif.
