%%
%% @doc This module has the function to delete a booking information.
%% This execute function will be called by MochiWeb request handler.
%% Also, the function is executed as the new process.
%% @author Yoichiro Tanaka
%%
-module(delete_booking).
-export([execute/3]).

%%
%% @doc Delete a booking information.
%% This function will be called from MochiWeb server process.
%% And, this function expects that a second argument has an ID of a booking
%% as the value of the key named "booking_id". The result of this process
%% will be sent to the process specified by the Pid argument, and
%% the result will have the flag whether the deleting booking is succeed or not.
%% If the user specified by the session is not the user who created the booking,
%% this deleting process will be failed.
%%
-spec(execute(pid(), ParameterList, string()) ->
             ok when
      ParameterList :: [Booking | tuple(),...],
      Booking :: {booking_id, string()}).
execute(Pid, Params, SessionId) ->
    BookingId = proplists:get_value("booking_id", Params),
    db:delete_booking(BookingId, SessionId),
    Pid ! {self(),
           200,
           {struct, [{result, true}]}},
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
            meck:expect(db, delete_booking,
                        fun(_BookingId, _SessionId) ->
                                ok
                        end),
            Pid = spawn(?MODULE,
                        execute,
                        [self(),
                         [{"booking_id", "id1"}],
                         "session1"]),
            receive
                Actual ->
                    ?assert(meck:validate(db)),
                    ?assert(meck:called(db, delete_booking,
                                        ["id1", "session1"])),
                    ?assertEqual({Pid, 200,
                                  {struct, [{result, true}]}},
                                 Actual)
            end
        end)}.

-endif.
