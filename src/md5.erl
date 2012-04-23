%%
%% @doc This module provides the function to generate a hashed string with MD5.
%% @author Yoichiro Tanaka
%%
-module(md5).
-export([md5_hex/1]).

%%
%% @doc Generate and return a hashed string with MD5.
%%
-spec(md5_hex(string()) ->
             string()).
md5_hex(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

%%
%% @doc Convert a list to a HEX string.
%%
-spec(list_to_hex(list()) ->
             string()).
list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

%%
%% @doc Convert an integer value to a HEX string.
%%
-spec(int_to_hex(integer()) ->
             string()).
int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

%%
%% @doc Convert an integer value to a charactor code.
%%
-spec(hex(integer()) ->
             integer()).
hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

md5_hex_test() ->
    ?assertEqual("7ac66c0f148de9519b8bd264312c4d64", md5_hex("abcdefg")),
    ?assertEqual("d41d8cd98f00b204e9800998ecf8427e", md5_hex("")),
    ok.

-endif.
