%%
%% @doc This module provides the function to retrieve bookings information.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(get_bookings).
-export([execute/3]).

-include("data.hrl").

%%
%% @doc Retrieve bookings.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has an ID of a category
%% as the value of the key named "category_id". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have multiple booking informations.Each booking have
%% the basic booking information and the user information who reserved the server.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [Category | tuple(),...],
      Category :: {category_id, string()}).
execute(Pid, Params, _SessionId) ->
    CategoryId = proplists:get_value("category_id", Params),
    Bookings = db:get_bookings(CategoryId),
    Response = lists:map(
                 fun(Booking) ->
                         Server = db:get_server(Booking#booking.server_id),
                         User = db:get_user(Booking#booking.user_id),
                         {struct,
                          [{id, list_to_binary(Booking#booking.id)},
                           {server_name, list_to_binary(Server#server.name)},
                           {from, list_to_binary(
                                    integer_to_list(Booking#booking.start_date))},
                           {to, list_to_binary(
                                  integer_to_list(Booking#booking.end_date))},
                           {branch, list_to_binary(Booking#booking.branch)},
                           {purpose, list_to_binary(Booking#booking.purpose)},
                           {user_id, list_to_binary(User#user.id)},
                           {user_name, list_to_binary(User#user.name)},
                           {user_email, list_to_binary(User#user.email)},
                           {user_irc, list_to_binary(User#user.irc)}]}
                 end, Bookings),
    Pid ! {self(),
           200,
           {struct, [{result, Response}]}}.

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
            Bookings = [
                        #booking{
                           id = "id1",
                           server_id = "server_id1",
                           user_id = "user_id1",
                           start_date = 20120308,
                           end_date = 20120309,
                           branch = "branch1",
                           purpose = "purpose1"},
                        #booking{
                                  id = "id2",
                                  server_id = "server_id2",
                                  user_id = "user_id2",
                                  start_date = 20120310,
                                  end_date = 20120311,
                                  branch = "branch2",
                                  purpose = "purpose2"}],
            Servers = [
                       #server{
                          id = "server_id1",
                          name = "server_name1"},
                       #server{
                                id = "server_id2",
                                name = "server_name2"}],
            Users = [
                     #user{
                        id = "user_id1",
                        name = "user_name1",
                        email = "user_email1",
                        irc = "user_irc1"},
                     #user{
                            id = "user_id2",
                            name = "user_name2",
                            email = "user_email2",
                            irc = "user_irc2"}],
            meck:expect(db, get_bookings, fun(_) -> Bookings end),
            meck:expect(db, get_server, fun(ServerId) ->
                                                case ServerId of
                                                    "server_id1" ->
                                                        lists:nth(1, Servers);
                                                    "server_id2" ->
                                                        lists:nth(2, Servers)
                                                end
                                        end),
            meck:expect(db, get_user, fun(UserId) ->
                                              case UserId of
                                                  "user_id1" ->
                                                      lists:nth(1, Users);
                                                  "user_id2" ->
                                                      lists:nth(2, Users)
                                              end
                                      end),
            Pid = spawn(?MODULE,
                        execute,
                        [self(), [{"category_id", "cat1"}], "session1"]),
            receive
                Actual ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid, 200,
                                  {struct, [{result,
                                             [{struct,
                                               [{id, <<"id1">>},
                                                {server_name, <<"server_name1">>},
                                                {from, <<"20120308">>},
                                                {to, <<"20120309">>},
                                                {branch, <<"branch1">>},
                                                {purpose, <<"purpose1">>},
                                                {user_id, <<"user_id1">>},
                                                {user_name, <<"user_name1">>},
                                                {user_email, <<"user_email1">>},
                                                {user_irc, <<"user_irc1">>}]},
                                              {struct,
                                               [{id, <<"id2">>},
                                                {server_name, <<"server_name2">>},
                                                {from, <<"20120310">>},
                                                {to, <<"20120311">>},
                                                {branch, <<"branch2">>},
                                                {purpose, <<"purpose2">>},
                                                {user_id, <<"user_id2">>},
                                                {user_name, <<"user_name2">>},
                                                {user_email, <<"user_email2">>},
                                                {user_irc, <<"user_irc2">>}]}]}]}},
                                 Actual)
            end
        end)}.

-endif.
