%%
%% @doc Generate the page which has avairable bookings today.
%% This module provides the function to retrieve all booking information and
%% send the page which has there bookings as the table.
%% The included booking includes today between the start_date and the end_date.
%% This page's format is the current it applied from 2010.
%%
-module(create_new_page).
-export([execute/1]).

-include("data.hrl").

%%
%% @doc Generate the page which has avairable bookings today.
%% The page will be sent to the process specified by the argument.
%%
-spec(execute(pid()) ->
             ok).
execute(Pid) ->
    Bookings = db:get_all_bookings(),
    Response = lists:foldl(
                 fun(Booking, Rows) ->
                         Server = db:get_server(Booking#booking.server_id),
                         User = db:get_user(Booking#booking.user_id),
                         lists:concat([Rows,
                                       "||",
                                       Server#server.name,
                                       "||",
                                       Booking#booking.branch,
                                       "||",
                                       Booking#booking.start_date,
                                       "-",
                                       Booking#booking.end_date,
                                       "||",
                                       User#user.name,
                                       "||",
                                       Booking#booking.purpose,
                                       "||",
                                       "\n"])
                 end,
                 [],
                 Bookings),
    Pid ! {self(),
           200,
           Response},
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
                        email = "user_email1"},
                     #user{
                            id = "user_id2",
                            name = "user_name2",
                            email = "user_email2"}],
            meck:expect(db, get_all_bookings, fun() -> Bookings end),
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
                        [self()]),
            receive
                Actual ->
                    ?assert(meck:validate(db)),
                    ?assertEqual({Pid, 200,
                                  lists:concat(
                                    [
                                     "||server_name1||branch1||20120308-20120309||",
                                     "user_name1||purpose1||\n",
                                     "||server_name2||branch2||20120310-20120311||",
                                     "user_name2||purpose2||\n"
                                    ])},
                                 Actual)
            end
        end)}.

-endif.
