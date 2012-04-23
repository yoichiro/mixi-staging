%%
%% @doc This module has the function to authenticate an user.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(authenticate).
-export([execute/3]).

%%
%% @doc Authenticate an user presented the user's credential.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has an email address 
%% as the value of the key named "email" and a password as the value of
%% the key named "password". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have the flag whether the creating user is succeed or not.
%% At the same time, if the creating user is succeed, the session against
%% the user will also be created.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [Email | Password | tuple(),...],
      Email :: {email, string()},
      Password :: {password, string()}).
execute(Pid, Params, SessionId) ->
    Email = proplists:get_value("email", Params),
    Password = proplists:get_value("password", Params),
    case db:authenticate(Email, Password) of
        true ->
            db:create_session(Email, SessionId),
            Pid ! {self(),
                   200,
                   {struct, [{result, true}]}};
        false ->
            Pid ! {self(),
                   200,
                   {struct, [{result, false}]}}
    end,
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
     [{"pattern1",
       [?_test(
           begin
               meck:expect(db, authenticate, fun(_Email, _Password) -> false end),
               Pid1 = spawn(?MODULE,
                            execute,
                            [self(),
                             [{"email", "email1"},
                              {"password", "password1"}],
                             "session1"]),
               receive
                   Actual ->
                       ?assert(meck:validate(db)),
                       ?assert(meck:called(db, authenticate,
                                           ["email1", "password1"])),
                       ?assertNot(meck:called(db, create_session,
                                              ["email1", "session1"])),
                       ?assertEqual({Pid1, 200, {struct, [{result, false}]}}, Actual)
               end
           end)]},
      {"pattern2",
       [?_test(
           begin
               meck:expect(db, authenticate, fun(_Email, _Password) -> true end),
               meck:expect(db, create_session, fun(_Email, _SessionId) -> ok end),
               Pid2 = spawn(?MODULE,
                            execute,
                            [self(),
                             [{"email", "email1"},
                              {"password", "password1"}],
                             "session1"]),
               receive
                   Actual ->
                       meck:validate(db),
                       ?assert(meck:called(db, authenticate,
                                           ["email1", "password1"])),
                       ?assert(meck:called(db, create_session,
                                           ["email1", "session1"])),
                       ?assertEqual({Pid2, 200, {struct, [{result, true}]}}, Actual)
               end
           end)]}]}.

-endif.
