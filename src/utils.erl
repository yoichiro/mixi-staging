%%
%% @doc This function provides convinience functions.
%% @author Yoichiro Tanaka
%%
-module(utils).
-export([create_date_integer/1]).

-include_lib("eunit/include/eunit.hrl").

%%
%% @doc This function returns a date value as an integer.
%% The format is 'yyyyMMdd'.
%%
-spec(create_date_integer(Date) ->
             integer() when
      Date :: {integer(), integer(), integer()}).
create_date_integer({Year, Month, Date}) ->
    list_to_integer(
      lists:concat([integer_to_list(Year),
                    zero_suppress(integer_to_list(Month), 2),
                    zero_suppress(integer_to_list(Date), 2)])).

%%
%% @doc Fill the '0' value, if the length of the Source value is less than Max.
%%
-spec(zero_suppress(list(), integer()) ->
             list()).
zero_suppress(Source, Max) ->
    Length = length(Source),
    if Length >= Max ->
            Source;
       true ->
            zero_suppress(lists:concat(["0", Source]), Max)
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

zero_suppress_test() ->
    ?assertEqual("01", zero_suppress("1", 2)),
    ?assertEqual("1", zero_suppress("1", 1)),
    ?assertEqual("00001", zero_suppress("1", 5)),
    ?assertEqual("1", zero_suppress("1", 0)),
    ?assertEqual("1", zero_suppress("1", -1)),
    ?assertEqual("00123", zero_suppress("123", 5)),
    ?assertEqual("123", zero_suppress("123", 1)),
    ok.

create_date_integer_test() ->
    ?assertEqual(20120305, create_date_integer({2012, 3, 5})),
    ?assertEqual(99999999, create_date_integer({9999, 99, 99})),
    ?assertEqual(0, create_date_integer({0, 0, 0})),
    ?assertEqual(10101, create_date_integer({1, 1, 1})),
    ok.

-endif.
