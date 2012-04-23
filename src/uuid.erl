%%
%% @doc This function provides an UUID string generating.
%% @author Yoichiro Tanaka
%%
-module(uuid).
-export([generate/0,
         to_string/1]).
-import(random).

%%
%% @doc Generates a random string UUID.
%%
-spec(generate() ->
             string()).
generate() ->
    to_string(v4()).

%%
%% @doc Generates a random binary UUID.
%%
-spec(v4() ->
             binary()).
v4() ->
    random:seed(now()),
    v4(random:uniform(round(math:pow(2, 48))) - 1,
       random:uniform(round(math:pow(2, 12))) - 1,
       random:uniform(round(math:pow(2, 32))) - 1,
       random:uniform(round(math:pow(2, 30))) - 1).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

%%
%% @doc Returns a string representation of a binary UUID.
%%
-spec(to_string(binary()) ->
             string()).
to_string(U) ->
    lists:flatten(
      io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
        get_parts(U))).

%%
%% @doc Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
%%
-spec(get_parts(binary()) ->
             list()).
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_test() ->
    [L1, L2, L3, L4, L5] = string:tokens(generate(), "-"),
    ?assertEqual(8, length(L1)),
    ?assertEqual(4, length(L2)),
    ?assertEqual(4, length(L3)),
    ?assertEqual(4, length(L4)),
    ?assertEqual(12, length(L5)),
    Result1 = generate(),
    Result2 = generate(),
    ?assertNotEqual(Result1, Result2),
    ok.

-endif.
