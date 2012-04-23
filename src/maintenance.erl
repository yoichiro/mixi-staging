%%
%% @doc This module provides some function to maintenance a database.
%% @author Yoichiro Tanaka
%%
-module(maintenance).

-export([create_tables/0,
         delete_tables/0,
         create_servers/0,
         delete_all_booking/0,
         delete_all_user/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("data.hrl").

%%
%% @doc Create tables.
%%
-spec(create_tables() ->
             ok).
create_tables() ->
    mnesia:create_table(
      user,
      [ {disc_only_copies, [node()]},
        {attributes, record_info(fields, user)} ]),
    mnesia:create_table(
      session,
      [ {disc_only_copies, [node()]},
        {attributes, record_info(fields, session)} ]),
    mnesia:create_table(
      category,
      [ {disc_only_copies, [node()]},
        {attributes, record_info(fields, category)} ]),
    mnesia:create_table(
      server,
      [ {disc_only_copies, [node()]},
        {attributes, record_info(fields, server)} ]),
    mnesia:create_table(
      booking,
      [ {disc_only_copies, [node()]},
        {attributes, record_info(fields, booking)} ]),
    ok.

%%
%% @doc Delete tables.
%% At the same time, all rows will also be deleted.
%%
-spec(delete_tables() ->
             ok).
delete_tables() ->
    mnesia:delete_table(user),
    mnesia:delete_table(session),
    mnesia:delete_table(category),
    mnesia:delete_table(server),
    mnesia:delete_table(booking),
    ok.
%%
%% @doc Insert rows of staging servers.
%%
-spec(create_servers() ->
             ok).
create_servers() ->
    Data = [
            ["PC - メイン",
             "staging-pc-01",
             "staging-pc-02"
            ],
            ["モバイル - メイン",
             "staging-mobile-01",
             "staging-mobile-02"
            ],
            ["アプリ - api.mixi-platform.com",
             "staging-api-01"
            ]
           ],
    create_category(Data),
    ok.

%%
%% @doc Create a new category.
%%
-spec(create_category(list()) ->
             ok).
create_category([]) ->
    ok;
create_category([Data | Tail]) ->
    [CategoryName | Servers] = Data,
    {ok, Category} = db:create_category(CategoryName),
    create_server(Category, Servers),
    create_category(Tail).

%%
%% @doc Create a new server.
%%
-spec(create_server(string(), list()) ->
             ok).
create_server(_, []) ->
    ok;
create_server(Category, [ServerName | Servers]) ->
    db:create_server(Category#category.id, ServerName),
    create_server(Category, Servers).

%%
%% @doc Delete all bookings
%%
-spec(delete_all_booking() ->
             ok).
delete_all_booking() ->
    Q = qlc:q([X || X <- mnesia:table(booking)]),
    Bookings = db:do(Q),
    F = fun() ->
                lists:foreach(fun(Booking) ->
                                      mnesia:delete_object(Booking)
                              end, Bookings)
        end,
    mnesia:transaction(F),
    ok.

%%
%% @doc Delete all users.
%%
-spec(delete_all_user() ->
             ok).
delete_all_user() ->
    Q = qlc:q([X || X <- mnesia:table(user)]),
    Users = db:do(Q),
    F = fun() ->
                lists:foreach(fun(User) ->
                                      mnesia:delete_object(User)
                              end, Users)
        end,
    mnesia:transaction(F),
    ok.
