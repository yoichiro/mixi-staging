%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for mixi_staging.

-module(mixi_staging_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

%%
%% @doc Initialize this web server.
%%
-spec(start(term()) ->
             term()).
start(Options) ->
    initialize(),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

%%
%% @doc Finish this web server.
%%
-spec(stop() ->
             ok).
stop() ->
    mochiweb_http:stop(?MODULE),
    cleanup(),
    ok.

%%
%% @doc Handle each requests and send a response.
%% If a GET request is received, the requested file will be sent.
%% And, if a POST request is received, it is recognized as an AJAX request,
%% and the JSON response will be sent.
%%
-spec(loop(term(), string()) ->
             ok).
loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                handle_get(Req, Path, DocRoot);
            'POST' ->
                handle_post(Req, Path, DocRoot);
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end,
    ok.

%% Internal API

initialize() ->
    mnesia:start(),
    ok.

cleanup() ->
    mnesia:stop(),
    ok.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options),
     proplists:delete(Option, Options)}.

handle_get(Req, "Giannutri", _DocRoot) ->
    send_old_page(Req);
handle_get(Req, "Staging2010", _DocRoot) ->
    send_new_page(Req);
handle_get(Req, "qrcode", _DocRoot) ->
    send_qrcode_image(Req);
handle_get(Req, Path, DocRoot) ->
    case Req:get_cookie_value("session_id") of
        undefined ->
            NewSessionId = uuid:generate(),
            SessionHeader = set_cookie("session_id", NewSessionId),
            Req:serve_file(Path, DocRoot, [SessionHeader]);
        _SessionId ->
            Req:serve_file(Path, DocRoot)
    end.

handle_post(Req, Path, _DocRoot) ->
    case Req:get_cookie_value("session_id") of
        undefined ->
            Req:respond({401, [{"Content-Type", "text/plain"}], "Unauthorized"});
        SessionId ->
            handle_ajax_request(Req, Path, SessionId)
    end.

handle_ajax_request(Req, Path, SessionId) ->
    try
        "ajax/" ++ Command = Path,
        {Status, Response} = route_process(Req, Command, SessionId),
        Encoder = mochijson2:encoder([{utf8, true}]),
        Req:respond({Status, [{"Content-Type", "application/json"}],
                     Encoder(Response)})
    catch
        error:{badmatch, _} ->
            Report = ["web request failed",
                      {path, Path},
                      {type, "error"},
                      {what, "badmatch"},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:not_found();
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

route_process(Req, Command, SessionId) ->
    process_flag(trap_exit, true),
    Params = Req:parse_post(),
    Pid = spawn_link(list_to_atom(Command), execute, [self(), Params, SessionId]),
    receive
        {Pid, Status, Response} ->
            {Status, Response};
        Other ->
            io:format("Error in route_process: ~p~n", [Other]),
            Message = "Command(" ++ Command ++ ") not found.",
            {404, {struct, [{code, 404},
                            {message, list_to_binary(Message)}]}}
    end.

set_cookie(Key, Value) ->
    mochiweb_cookies:cookie(Key, Value, [{path, "/"}]).

send_old_page(Req) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(create_old_page, execute, [self()]),
    receive
        {Pid, Status, Response} ->
            Req:respond(
              {Status,
               [{"Content-Type", "text/plain; charset=UTF-8"}],
               Response})
    end.

send_new_page(Req) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(create_new_page, execute, [self()]),
    receive
        {Pid, Status, Response} ->
            Req:respond(
              {Status,
               [{"Content-Type", "text/plain; charset=UTF-8"}],
               Response})
    end.

send_qrcode_image(Req) ->
    process_flag(trap_exit, true),
    Params = Req:parse_qs(),
    Pid = spawn_link(create_qrcode_image, execute, [self(), Params]),
    receive
	{Pid, Status, Response} ->
	    Req:respond(
	      {Status,
	       [{"Content-Type", "image/png"}],
	       Response})
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    meck:new([mnesia, mochiweb_http]),
    meck:expect(mnesia, start, fun() -> ok end),
    meck:expect(mochiweb_http, start, fun(_A) -> ok end),
    start([{docroot, "docroot1"}, {k1, v1}]),
    ?assert(meck:validate(mnesia)),
    ?assert(meck:validate(mochiweb_http)),
    ?assert(meck:called(mnesia, start, [])),
    ?assertMatch([{_, {mochiweb_http, start,
                            [[{name, mixi_staging_web},
                              {loop, _},
                              {k1, v1}]]},
                   ok}],
                 meck:history(mochiweb_http, self())),
    meck:unload([mnesia, mochiweb_http]),
    ok.

stop_test() ->
    meck:new([mnesia, mochiweb_http]),
    meck:expect(mnesia, stop, fun() -> ok end),
    meck:expect(mochiweb_http, stop, fun(_M) -> ok end),
    stop(),
    meck:called(mnesia, stop, []),
    meck:called(mochiweb_http, stop, [?MODULE]),
    meck:unload([mnesia, mochiweb_http]),
    ok.

initialize_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, start, fun() -> ok end),
    initialize(),
    ?assert(meck:called(mnesia, start, [])),
    meck:unload(mnesia),
    ok.

cleanup_test() ->
    meck:new(mnesia),
    meck:expect(mnesia, stop, fun() -> ok end),
    cleanup(),
    ?assert(meck:called(mnesia, stop, [])),
    meck:unload(mnesia),
    ok.

get_option_test() ->
    Options = [{docroot, "docroot1"}, {k1, v1}, {k2, v2}],
    Actual = get_option(docroot, Options),
    ?assertEqual({"docroot1", [{k1, v1}, {k2, v2}]}, Actual),
    ok.

send_old_page_test() ->
    meck:new([db, mochiweb_request]),
    meck:expect(mochiweb_request, respond, fun(_) -> ok end),
    meck:expect(db,
                get_all_bookings,
                fun() ->
                        []
                end),
    send_old_page(mochiweb_request),
    ?assert(meck:called(mochiweb_request, respond,
                        [{200,
                          [{"Content-Type", "text/plain; charset=UTF-8"}],
                          []}])),
    meck:unload([db, mochiweb_request]),
    ok.

send_new_page_test() ->
    meck:new([db, mochiweb_request]),
    meck:expect(mochiweb_request, respond, fun(_) -> ok end),
    meck:expect(db,
                get_all_bookings,
                fun() ->
                        []
                end),
    send_new_page(mochiweb_request),
    ?assert(meck:called(mochiweb_request, respond,
                        [{200,
                          [{"Content-Type", "text/plain; charset=UTF-8"}],
                          []}])),
    meck:unload([db, mochiweb_request]),
    ok.

-endif.
