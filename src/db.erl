%%
%% @doc This module has some functions to manipulate each entites in the database.
%% This client can retrieve, store and update entities of a category, server,
%% user, session and booking from Mnesia. Basically, each functions are executed in a
%% transaction.
%% @author Yoichiro Tanaka
%%
-module(db).

%% for category
-export([create_category/1,
         get_categories/0,
         delete_category/1]).

%% for server
-export([create_server/2,
         get_server/1,
         get_servers/1,
         delete_server/1]).

%% for user
-export([create_user/4,
         authenticate/2,
         get_user_by_session_id/1,
         get_user/1]).

%% for session
-export([create_session/2,
         delete_session/1]).

%% for booking
-export([reserve/6,
         get_bookings/1,
         delete_booking/2,
         get_booking/1,
         get_all_bookings/0]).

%% other
-export([do/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("data.hrl").


%% for category

%%
%% @doc Retrieve all categories.
%% This result list is sorted by the category's name.
%% Also, the category which its deleted flag is true is reduced from this result.
%%
-spec(get_categories() ->
             [#category{}]).
get_categories() ->
    Q1 = qlc:q([X || X <- mnesia:table(category),
                     X#category.deleted == false]),
    Q2 = qlc:sort(
           Q1,
           {order, fun(Cat1, Cat2) ->
                           Cat1#category.name < Cat2#category.name
                   end}),
    do(Q2).

%%
%% @doc Create a new category into the database.
%% The object of the category record is actually generated and stored.
%% The created category has the specified name.
%%
-spec(create_category(string()) ->
             {ok, #category{}}).
create_category(Name) ->
    ID = uuid:generate(),
    Category = #category{
      id = ID,
      name = Name,
      deleted = false
     },
    replace_entity(Category),
    {ok, Category}.

%%
%% @doc Delete a category from the database.
%% Actually, the specified category is not deleted,
%% instead the deleted flag is changed to true.
%% As this result, the deleted record will be return.
%%
-spec(delete_category(string()) ->
             {ok, #category{}} | {error, string()}).
delete_category(CategoryId) ->
    Q = qlc:q([X || X <- mnesia:table(category),
                    X#category.id == CategoryId]),
    Category = get_one_entity(Q),
    DeletedCategory = Category#category{deleted = true},
    replace_entity(DeletedCategory).


%% for server

%%
%% @doc Create a new server into the database.
%% The server is created depending the category specified by the Category ID.
%%
-spec(create_server(string(), string()) ->
             {ok, #server{}}).
create_server(CategoryId, Name) ->
    ID = uuid:generate(),
    Server = #server{
      id = ID,
      category_id = CategoryId,
      name = Name,
      deleted = false
     },
    replace_entity(Server),
    {ok, Server}.

%%
%% @doc Retrieve all IDs of servers which belong in the specified category.
%% The server which its deleted flag is true will be reduced.
%%
-spec(get_all_server_ids(string()) ->
             [string()]).
get_all_server_ids(CategoryId) ->
    Q = qlc:q([X#server.id || X <- mnesia:table(server),
                              X#server.category_id == CategoryId,
                              X#server.deleted == false]),
    do(Q).

%%
%% @doc Retrieve a server which has the specified server ID.
%% If the server has deleted (in other words, its deleted flag is true),
%% it will not be returned.
%% Also, if multiple servers (has same server ID) exist, an error will be occurred.
%%
-spec(get_server(string()) ->
             #server{} | not_found | {error, string()}).
get_server(ServerId) ->
    Q = qlc:q([X || X <- mnesia:table(server),
                    X#server.id == ServerId,
                    X#server.deleted == false]),
    get_one_entity(Q).

%%
%% @doc Retrieve all servers belonging the specified category.
%% This result list is sorted by the server's name.
%% Also, the server which its deleted flag is true is reduced from this result.
%%
-spec(get_servers(string()) ->
             [#server{}]).
get_servers(CategoryId) ->
    Q1 = qlc:q([X || X <- mnesia:table(server),
                     X#server.category_id == CategoryId,
                     X#server.deleted == false]),
    Q2 = qlc:sort(
           Q1,
           {order, fun(S1, S2) ->
                           S1#server.name < S2#server.name
                   end}),
    do(Q2).

%%
%% @doc Delete a server from the database.
%% Actually, the specified server is not deleted,
%% instead the deleted flag is changed to true.
%% As this result, the deleted record will be return.
%%
-spec(delete_server(string()) ->
             {ok, #server{}} | {error, string()}).
delete_server(ServerId) ->
    Q = qlc:q([X || X <- mnesia:table(server),
                    X#server.id == ServerId]),
    Server = get_one_entity(Q),
    F = fun() ->
                DeletedServer = Server#server{deleted = true},
                replace_entity(DeletedServer)
        end,
    mnesia:transaction(F).


%% for user

%%
%% @doc Create a new user into the database.
%% If the user which has the same email address in the database,
%% the error which presents this situation will be occurred.
%%
-spec(create_user(string(), string(), string(), string()) ->
             {ok, #user{}} | {already_exists, #user{}}).
create_user(Name, Email, Password, Irc) ->
    case get_user_by_email(Email) of
        not_found ->
            ID = uuid:generate(),
            PasswordHash = md5:md5_hex(Password),
            NewUser = #user{
              id = ID,
              name = Name,
              email = Email,
              password = PasswordHash,
              irc = Irc
             },
            replace_entity(NewUser),
            {ok, NewUser};
        User ->
            {already_exists, User}
    end.

%%
%% @doc Authenticate by the user's credential.
%% The credential is composed of a email and password.
%% The password is stored as the md5 hashed string, and is cofirmed by
%% comparing each hashed them.
%%
-spec(authenticate(string(), string()) ->
             boolean()).
authenticate(Email, Password) ->
    case get_user_by_email(Email) of
        not_found ->
            false;
        User ->
            PasswordHash = md5:md5_hex(Password),
            User#user.password == PasswordHash
    end.

%%
%% @doc Retrieve an user which has the specified email address.
%%
-spec(get_user_by_email(string()) ->
             #user{} | not_found | {error, string()}).
get_user_by_email(Email) ->
    Q = qlc:q([X || X <- mnesia:table(user),
                    X#user.email == Email]),
    get_one_entity(Q).


%%
%% @doc Retrieve an user related the specified session.
%% One session is related to one user in the database.
%%
-spec(get_user_by_session_id(string()) ->
             #user{} | not_found | {error, string()}).
get_user_by_session_id(SessionId) ->
    Q = qlc:q([Y || X <- mnesia:table(session),
                    Y <- mnesia:table(user),
                    X#session.user_id == Y#user.id,
                    X#session.id == SessionId]),
    get_one_entity(Q).

%%
%% @doc Retrieve an user which has the specified user ID.
%%
-spec(get_user(string()) ->
             #user{} | not_found | {error, string()}).
get_user(UserId) ->
    Q = qlc:q([X || X <- mnesia:table(user),
                    X#user.id == UserId]),
    get_one_entity(Q).


%% for session

%%
%% @doc Create a new session related to an user specified by the email address.
%% This function will be used at a logged-in process.
%%
-spec(create_session(string(), string()) ->
             {ok, #session{}} | {error, string()}).
create_session(Email, SessionId) ->
    F = fun() ->
                User = get_user_by_email(Email),
                NewSession = #session{
                  id = SessionId,
                  user_id = User#user.id
                 },
                replace_entity(NewSession)
        end,
    mnesia:transaction(F).

%%
%% @doc Delete a session from the database.
%% This function actually deletes the session specified
%% by the session ID from the database.
%%
-spec(delete_session(string()) ->
             ok).
delete_session(SessionId) ->
    F = fun() ->
                mnesia:delete({session, SessionId})
        end,
    mnesia:transaction(F).


%% for booking

%%
%% @doc Reserve a server according to some conditions.
%% First, this function finds the server which will not be busy at the period
%% the client specifies. If there are one or more such servers,
%% this function creates a booking record to the database.
%% If there is not such server, the error will be return.
%% Two arguments "From" and "To" must present the date formatted "YYYYMMDD".
%%
-spec(reserve(integer(), integer(), string(), string(), string(), string()) ->
             {ok, #booking{}} | available_server_not_found).
reserve(From, To, Branch, Purpose, CategoryId, SessionId) ->
    AllServerIds = get_all_server_ids(CategoryId),
    Q = qlc:q([X#booking.server_id || X <- mnesia:table(booking),
                                      ((X#booking.start_date =< From)
                                       and (From =< X#booking.end_date))
                                          orelse
                                            ((X#booking.start_date =< To)
                                             and (To =< X#booking.end_date))
                                          orelse
                                            ((From =< X#booking.start_date)
                                             and (X#booking.end_date =< To))]),
    BookedServerIds = do(Q),
    case lists:subtract(AllServerIds, BookedServerIds) of
        [] ->
            available_server_not_found;
        AvaliableServerIds ->
            [ServerId | _] = AvaliableServerIds,
            User = get_user_by_session_id(SessionId),
            create_booking(ServerId, From, To, Branch, Purpose, User#user.id)
    end.

%%
%% @doc Create a booking record into the database.
%%
-spec(create_booking(string(), integer(), integer(), string(), string(), string()) ->
             {ok, #booking{}}).
create_booking(ServerId, From, To, Branch, Purpose, UserId) ->
    ID = uuid:generate(),
    Booking = #booking{
      id = ID,
      server_id = ServerId,
      start_date = From,
      end_date = To,
      branch = Branch,
      purpose = Purpose,
      user_id = UserId},
    replace_entity(Booking),
    {ok, Booking}.

%%
%% @doc Retrieve bookings belonging to the specified category.
%% The server which has already been deleted is not included in this result.
%% And, if the end date which the booking has is before than today,
%% it is also reduce from the result. The result list is sorted by
%% the start date.
%%
-spec(get_bookings(string()) ->
             [#booking{}]).
get_bookings(CategoryId) ->
    Q1 = qlc:q([X#server.id || X <- mnesia:table(server),
                               X#server.category_id == CategoryId,
                               X#server.deleted == false]),
    ServerIds = do(Q1),
    Q2 = qlc:q([X || X <- mnesia:table(booking),
                     lists:member(X#booking.server_id, ServerIds),
                     X#booking.end_date >= utils:create_date_integer(date())]),
    Q3 = qlc:sort(
           Q2,
           {order, fun(Book1, Book2) ->
                           Book1#booking.start_date < Book2#booking.start_date
                   end}),
    do(Q3).

%%
%% @doc Retrieve a booking information with the specified booking ID.
%%
-spec(get_booking(string()) ->
             #booking{} | not_found | {error, string()}).
get_booking(BookingId) ->
    Q = qlc:q([X || X <- mnesia:table(booking),
                    X#booking.id == BookingId]),
    get_one_entity(Q).

%%
%% @doc Delete a booking with the booking ID.
%% If the target booking information has not been created by the user
%% of this session, this function will be failed.
%%
-spec(delete_booking(string(), string()) ->
             boolean()).
delete_booking(BookingId, SessionId) ->
    Booking = get_booking(BookingId),
    User = get_user_by_session_id(SessionId),
    case Booking#booking.user_id == User#user.id of
        true ->
            F = fun() ->
                        mnesia:delete_object(Booking)
                end,
            mnesia:transaction(F),
            true;
        false ->
            false
    end.

%%
%% @doc Retrieve all bookings which is avairable today.
%%
-spec(get_all_bookings() ->
             [#booking{}]).
get_all_bookings() ->
    Today = utils:create_date_integer(date()),
    Q = qlc:q([X || X <- mnesia:table(booking),
                    X#booking.start_date =< Today,
                    X#booking.end_date >= Today]),
    do(Q).


%% utility functions

%%
%% @doc Execute a query and return the result in a transaction.
%% If the result count of the query are two or more,
%% this function will return the error.
%%
-spec(get_one_entity(QH) ->
             term() | not_found | {error, string} when
      QH :: qlc:query_handle()).
get_one_entity(Q) ->
    ResultList = do(Q),
    case length(ResultList) of
        0 ->
            not_found;
        1 ->
            [Result | _] = ResultList,
            Result;
        _ ->
            {error, "Many same objects exists."}
    end.

%%
%% @doc Execute a query and return results in a transaction.
%% This function will return the list which has result entities of the query.
%%
-spec(do(QH) ->
             term() when
      QH :: qlc:query_handle()).
do(Q) ->
    F = fun() ->
                qlc:e(Q)
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

%%
%% @doc Insert or update an entity in a transaction.
%% For instance, this function calls the mnesia:write() function simplify
%% with the entity this client specified.
%%
-spec(replace_entity(term()) ->
             {ok, term()} | {error, term()}).
replace_entity(Entity) ->
    F = fun() ->
                mnesia:write(Entity)
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            {ok, Entity};
        Other ->
            {error, Other}
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_categories_test() ->
    meck:new(mnesia),
    Categories = [#category{
                     id = "id3",
                     name = "category3",
                     deleted = false},
                  #category{
                             id = "id2",
                             name = "category2",
                             deleted = true},
                  #category{
                             id = "id1",
                             name = "category1",
                             deleted = false}],
    meck:expect(mnesia, table, fun(_Tab) -> Categories end),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    Actual = get_categories(),
    ?assertEqual(2, length(Actual)),
    [Cat1, Cat3] = Actual,
    ?assertEqual(lists:nth(3, Categories), Cat1),
    ?assertEqual(lists:nth(1, Categories), Cat3),
    meck:validate(mnesia),
    meck:unload(mnesia),
    ok.

create_category_test() ->
    meck:new([mnesia, uuid]),
    meck:expect(uuid, generate, fun() -> "id1" end),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    meck:expect(mnesia, write, fun(_E) -> ok end),
    Actual = create_category("name1"),
    meck:validate(mnesia),
    meck:validate(uuid),
    ?assertEqual({ok, #category{id = "id1", name = "name1", deleted = false}},
                 Actual),
    meck:called(mnesia, write, [#category{id = "id1",
                                          name = "name1",
                                          deleted = false}]),
    meck:unload([mnesia, uuid]),
    ok.

delete_category_test() ->
    meck:new(mnesia),
    Category = #category{
      id = "id1",
      name = "name1",
      deleted = false},
    Categories = [Category],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Categories
                               end),
    meck:expect(mnesia, transaction, fun(F) ->
                                             {atomic, F()}
                                     end),
    meck:expect(mnesia, write, fun(_E) ->
                                       ok
                               end),
    Actual = delete_category("id1"),
    ?assertEqual({ok, #category{id = "id1",
                                name = "name1",
                                deleted = true}},
                 Actual),
    meck:validate(mnesia),
    meck:unload(mnesia),
    ok.

create_server_test() ->
    meck:new([mnesia, uuid]),
    meck:expect(uuid, generate, fun() -> "id1" end),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    meck:expect(mnesia, write, fun(_E) -> ok end),
    Actual = create_server("cat1", "name1"),
    meck:validate(mnesia),
    meck:validate(uuid),
    ?assertEqual({ok, #server{id = "id1",
                              category_id = "cat1",
                              name = "name1",
                              deleted = false}},
                 Actual),
    meck:called(mnesia, write, [#server{id = "id1",
                                        category_id = "cat1",
                                        name = "name1",
                                        deleted = false}]),
    meck:unload([mnesia, uuid]),
    ok.

get_all_server_ids_test() ->
    meck:new(mnesia),
    Servers = [
               #server{
                  id = "id1",
                  category_id = "cat1",
                  name = "name1",
                  deleted = false},
               #server{
                  id = "id2",
                  category_id = "cat1",
                  name = "name2",
                  deleted = true},
               #server{
                  id = "id3",
                  category_id = "cat2",
                  name = "name3",
                  deleted = false},
               #server{
                  id = "id4",
                  category_id = "cat2",
                  name = "name4",
                  deleted = true}],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Servers
                               end),
    meck:expect(mnesia, transaction, fun(F) ->
                                             {atomic, F()}
                                     end),
    Actual = get_all_server_ids("cat1"),
    meck:validate(mnesia),
    ?assertEqual(["id1"], Actual),
    meck:unload(mnesia),
    ok.

get_server_test() ->
    meck:new(mnesia),
    Server1 = #server{
      id = "id1",
      category_id = "cat1",
      name = "name1",
      deleted = false},
    Servers = [Server1,
               #server{
                 id = "id2",
                 category_id = "cat1",
                 name = "name2",
                 deleted = true}],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Servers
                               end),
    meck:expect(mnesia, transaction, fun(F) ->
                                             {atomic, F()}
                                     end),
    Actual1 = get_server("id1"),
    ?assertEqual(Server1, Actual1),
    Actual2 = get_server("id2"),
    ?assertEqual(not_found, Actual2),
    meck:validate(mnesia),
    meck:unload(mnesia),
    ok.

get_servers_test() ->
    meck:new(mnesia),
    Servers = [
               #server{
                  id = "id2",
                  category_id = "cat1",
                  name = "name2",
                  deleted = true},
               #server{
                  id = "id3",
                  category_id = "cat1",
                  name = "name3",
                  deleted = false},
               #server{
                  id = "id1",
                  category_id = "cat1",
                  name = "name1",
                  deleted = false},
               #server{
                  id = "id3",
                  category_id = "cat2",
                  name = "name3",
                  deleted = false}],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Servers
                               end),
    meck:expect(mnesia, transaction, fun(F) ->
                                             {atomic, F()}
                                     end),
    Actual = get_servers("cat1"),
    meck:validate(mnesia),
    ?assertEqual(2, length(Actual)),
    [Actual1, Actual2] = Actual,
    ?assertEqual(#server{
                    id = "id1",
                    category_id = "cat1",
                    name = "name1",
                    deleted = false
                   },
                 Actual1),
    ?assertEqual(#server{
                    id = "id3",
                    category_id = "cat1",
                    name = "name3",
                    deleted = false
                   },
                 Actual2),
    meck:unload(mnesia),
    ok.

delete_server_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    Servers = [
               #server{
                  id = "id1",
                  category_id = "cat1",
                  name = "name1",
                  deleted = false}],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Servers
                               end),
    meck:expect(mnesia, write, fun(_E) -> ok end),
    delete_server("id1"),
    meck:validate(mnesia),
    meck:called(mnesia, write, [#server{id = "id1",
                                        category_id = "cat1",
                                        name = "name1",
                                        deleted = true}]),
    meck:unload(mnesia),
    ok.

create_user_test() ->
    meck:new([mnesia, uuid, md5]),
    meck:expect(uuid, generate, fun() ->
                                        "id1"
                                end),
    meck:expect(md5, md5_hex, fun(_A) ->
                                      "hashed1"
                              end),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    User = #user{
      id = "id1",
      name = "name1",
      email = "email1",
      password = "password1",
      irc = "irc1"},
    Users = [User],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Users
                               end),
    meck:expect(mnesia, write, fun(_E) -> ok end),
    Actual1 = create_user("name2", "email2", "password2", "irc2"),
    ?assertEqual({ok, #user{
                    id = "id1",
                    name = "name2",
                    email = "email2",
                    password = "hashed1",
                    irc = "irc2"}},
                 Actual1),
    meck:called(mnesia, write, [#user{
                                   id = "id1",
                                   name = "name2",
                                   email = "email2",
                                   password = "hashed1",
                                   irc = "irc1"}]),
    Actual2 = create_user("name1", "email1", "password1", "irc1"),
    ?assertEqual({already_exists, User}, Actual2),
    meck:unload([mnesia, uuid, md5]),
    ok.

authenticate_test() ->
    meck:new([mnesia, md5]),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    User = #user{
      id = "id1",
      name = "name1",
      email = "email1",
      password = "hashed1",
      irc = "irc1"},
    Users = [User],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Users
                               end),
    Actual1 = authenticate("email2", ""),
    ?assertEqual(false, Actual1),
    meck:expect(md5, md5_hex, fun(_A) ->
                                      "hashed1"
                               end),
    Actual2 = authenticate("email1", "password1"),
    ?assertEqual(true, Actual2),
    meck:called(md5, md5_hex, ["password1"]),
    meck:expect(md5, md5_hex, fun(_A) ->
                                      "hashed2"
                               end),
    Actual3 = authenticate("email1", "password1"),
    ?assertEqual(false, Actual3),
    meck:unload([mnesia, md5]),
    ok.

get_user_by_email_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    User = #user{
      id = "id1",
      name = "name1",
      email = "email1",
      password = "password1",
      irc = "irc1"},
    Users = [User],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Users
                               end),
    Actual1 = get_user_by_email("email1"),
    ?assertEqual(User, Actual1),
    Actual2 = get_user_by_email("email2"),
    ?assertEqual(not_found, Actual2),
    meck:unload(mnesia),
    ok.

get_user_by_session_id_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    User = #user{
      id = "user_id1",
      name = "name1",
      email = "email1",
      password = "password1",
      irc = "irc1"},
    Users = [User],
    Session = #session{
      id = "session_id1",
      user_id = "user_id1"},
    Sessions = [Session],
    meck:expect(mnesia, table, fun(Tab) ->
                                       case Tab of
                                           session ->
                                               Sessions;
                                           user ->
                                               Users
                                       end
                               end),
    Actual1 = get_user_by_session_id("session_id1"),
    ?assertEqual(User, Actual1),
    Actual2 = get_user_by_session_id("session_id2"),
    ?assertEqual(not_found, Actual2),
    meck:unload(mnesia),
    ok.

get_user_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    User = #user{
      id = "id1",
      name = "name1",
      email = "email1",
      password = "password1",
      irc = "irc1"},
    Users = [User],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Users
                               end),
    Actual1 = get_user("id1"),
    ?assertEqual(User, Actual1),
    Actual2 = get_user("id2"),
    ?assertEqual(not_found, Actual2),
    meck:unload(mnesia),
    ok.

create_session_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    meck:expect(mnesia, write, fun(_E) -> {atomic, ok} end),
    User = #user{
      id = "id1",
      name = "name1",
      email = "email1",
      password = "password1",
      irc = "irc1"},
    Users = [User],
    meck:expect(mnesia, table, fun(_Tab) ->
                                       Users
                               end),
    create_session("email1", "session_id1"),
    meck:called(mnesia, write, [#session{
                                   id = "session_id1",
                                   user_id = "id1"}]),
    meck:unload(mnesia),
    ok.

delete_session_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    meck:expect(mnesia, delete, fun(_E) -> ok end),
    delete_session("session_id1"),
    meck:called(mnesia, delete, [{session, "session_id1"}]),
    meck:unload(mnesia),
    ok.

reserve_test() ->
    meck:new([mnesia, uuid]),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    Servers = [
               #server{
                  id = "server_id1",
                  category_id = "cat1",
                  name = "name1",
                  deleted = false},
               #server{
                        id = "server_id2",
                        category_id = "cat1",
                        name = "name2",
                        deleted = false}],
    Bookings = [
                #booking{
                   id = "id1",
                   server_id = "server_id1",
                   start_date = 20120308,
                   end_date = 20120309,
                   branch = "branch1",
                   purpose = "purpose1",
                   user_id = "user_id1"},
               #booking{
                         id = "id2",
                         server_id = "server_id2",
                         start_date = 20120308,
                         end_date = 20120309,
                         branch = "branch2",
                         purpose = "purpose2",
                         user_id = "user_id1"}],
    Users = [
             #user{
                id = "user_id1",
                name = "name1",
                email = "email1",
                password = "hashed1",
                irc = "irc1"}],
    Sessions = [
                #session{
                   id = "session1",
                   user_id = "user_id1"}],
    meck:expect(mnesia, table, fun(Tab) ->
                                       case Tab of
                                           server ->
                                               Servers;
                                           booking ->
                                               Bookings;
                                           user ->
                                               Users;
                                           session ->
                                               Sessions
                                       end
                               end),
    meck:expect(uuid, generate, fun() -> "id3" end),
    meck:expect(mnesia, write, fun(_E) -> {atomic, ok} end),
    Actual1 = reserve(20120310, 20120311, "branch1", "purpose1", "cat1", "session1"),
    Expected = #booking{
      id = "id3",
      server_id = "server_id1",
      start_date = 20120310,
      end_date = 20120311,
      branch = "branch1",
      purpose = "purpose1",
      user_id = "user_id1"},
    ?assertEqual({ok, Expected}, Actual1),
    meck:called(mnesia, write, [Expected]),
    Actual2 = reserve(20120309, 20120311, "branch1", "purpose1", "cat1", "session1"),
    ?assertEqual(available_server_not_found, Actual2),
    Actual3 = reserve(20120301, 20120308, "branch1", "purpose1", "cat1", "session1"),
    ?assertEqual(available_server_not_found, Actual3),
    Actual4 = reserve(20120301, 20120311, "branch1", "purpose1", "cat1", "session1"),
    ?assertEqual(available_server_not_found, Actual4),
    meck:unload([mnesia, uuid]),
    ok.

create_booking_test() ->
    meck:new([mnesia, uuid]),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    meck:expect(mnesia, write, fun(_E) -> {atomic, ok} end),
    meck:expect(uuid, generate, fun() -> "id1" end),
    Expected = #booking{
      id = "id1",
      server_id = "server_id1",
      start_date = 20120308,
      end_date = 20120309,
      branch = "branch1",
      purpose = "purpose1",
      user_id = "user_id1"},
    Actual = create_booking("server_id1",
                            20120308,
                            20120309,
                            "branch1",
                            "purpose1",
                            "user_id1"),
    ?assertEqual({ok, Expected}, Actual),
    meck:validate(mnesia),
    meck:called(mnesia, write, [Expected]),
    meck:unload([mnesia, uuid]),
    ok.

get_bookings_test() ->
    meck:new([mnesia, utils]),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    Servers = [
               #server{
                  id = "server_id1",
                  category_id = "cat1",
                  name = "name1",
                  deleted = false}],
    Booking2 = #booking{
      id = "id2",
      server_id = "server_id1",
      start_date = 20120308,
      end_date = 20120309,
      branch = "branch2",
      purpose = "purpose2",
      user_id = "user_id1"},
    Booking3 = #booking{
      id = "id3",
      server_id = "server_id1",
      start_date = 20120305,
      end_date = 20120309,
      branch = "branch3",
      purpose = "purpose3",
      user_id = "user_id2"},
    Bookings = [
                #booking{
                   id = "id1",
                   server_id = "server_id1",
                   start_date = 20120306,
                   end_date = 20120307,
                   branch = "branch1",
                   purpose = "purpose1",
                   user_id = "user_id1"},
               Booking2,
               Booking3],
    meck:expect(mnesia, table, fun(Tab) ->
                                       case Tab of
                                           server ->
                                               Servers;
                                           booking ->
                                               Bookings
                                       end
                               end),
    meck:expect(utils, create_date_integer, fun(_A) -> 20120308 end),
    Actual = get_bookings("cat1"),
    ?assertEqual(2, length(Actual)),
    ?assertEqual([Booking3, Booking2], Actual),
    meck:validate(mnesia),
    meck:unload([mnesia, utils]),
    ok.

get_booking_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    Expected = #booking{
      id = "id1",
      server_id = "server_id1",
      start_date = 20120306,
      end_date = 20120307,
      branch = "branch1",
      purpose = "purpose1",
      user_id = "user_id1"},
    Bookings = [Expected],
    meck:expect(mnesia, table, fun(_Tab) -> Bookings end),
    Actual = get_booking("id1"),
    ?assertEqual(Expected, Actual),
    meck:validate(mnesia),
    meck:unload(mnesia),
    ok.

delete_booking_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    meck:expect(mnesia, delete_object, fun(_E) -> ok end),
    Expected = #booking{
      id = "id1",
      server_id = "server_id1",
      start_date = 20120306,
      end_date = 20120307,
      branch = "branch1",
      purpose = "purpose1",
      user_id = "user_id1"},
    Bookings = [
                Expected,
                #booking{
                  id = "id2",
                  server_id = "server_id2",
                  start_date = 20120306,
                  end_date = 20120307,
                  branch = "branch2",
                  purpose = "purpose2",
                  user_id = "user_id2"}],
    Users = [
             #user{
                id = "user_id1",
                name = "name1",
                email = "email1",
                password = "hashed1",
                irc = "irc1"}],
    Sessions = [
                #session{
                   id = "session1",
                   user_id = "user_id1"}],
    meck:expect(mnesia, table, fun(Tab) ->
                                       case Tab of
                                           booking ->
                                               Bookings;
                                           user ->
                                               Users;
                                           session ->
                                               Sessions
                                       end
                               end),
    Actual1 = delete_booking("id1", "session1"),
    ?assert(Actual1),
    meck:called(mnesia, delete_object, [Expected]),
    Actual2 = delete_booking("id2", "session1"),
    ?assertNot(Actual2),
    meck:unload(mnesia),
    ok.

get_all_bookings_test() ->
    meck:new([mnesia, utils]),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    meck:expect(utils, create_date_integer, fun(_A) -> 20120309 end),
    Expected = #booking{
      id = "id1",
      server_id = "server_id1",
      start_date = 20120308,
      end_date = 20120310,
      branch = "branch1",
      purpose = "purpose1",
      user_id = "user_id1"},
    Bookings = [
                Expected,
                #booking{
                  id = "id2",
                  server_id = "server_id2",
                  start_date = 20120306,
                  end_date = 20120308,
                  branch = "branch2",
                  purpose = "purpose2",
                  user_id = "user_id2"}],
    meck:expect(mnesia, table, fun(_Tab) -> Bookings end),
    Actual = get_all_bookings(),
    ?assertEqual(1, length(Actual)),
    ?assertEqual([Expected], Actual),
    meck:validate(mnesia),
    meck:unload([mnesia, utils]),
    ok.

do_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    Q = qlc:q([X * 2 || X <- [1, 2, 3]]),
    Actual = do(Q),
    ?assertEqual([2, 4, 6], Actual),
    meck:unload(mnesia),
    ok.

replace_entity_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> F() end),
    meck:expect(mnesia, write, fun(_E) -> {atomic, ok} end),
    Entity = 123,
    Actual1 = replace_entity(Entity),
    ?assertEqual({ok, Entity}, Actual1),
    meck:validate(mnesia),
    meck:called(mnesia, write, [Entity]),
    meck:expect(mnesia, transaction, fun(_F) -> error1 end),
    Actual2 = replace_entity(Entity),
    ?assertEqual({error, error1}, Actual2),
    meck:validate(mnesia),
    meck:unload(mnesia),
    ok.

get_one_entity_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, transaction, fun(F) -> {atomic, F()} end),
    Q1 = qlc:q([X || X <- []]),
    Actual1 = get_one_entity(Q1),
    ?assertEqual(not_found, Actual1),
    Q2 = qlc:q([X || X <- [1]]),
    Actual2 = get_one_entity(Q2),
    ?assertEqual(1, Actual2),
    Q3 = qlc:q([X || X <- [1, 2]]),
    Actual3 = get_one_entity(Q3),
    ?assertEqual({error, "Many same objects exists."}, Actual3),
    meck:unload(mnesia),
    ok.

-endif.
