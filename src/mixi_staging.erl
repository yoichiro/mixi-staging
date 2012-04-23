%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mixi_staging.

-module(mixi_staging).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mixi_staging server.
start() ->
    mixi_staging_deps:ensure(),
    ensure_started(crypto),
    application:start(mixi_staging).


%% @spec stop() -> ok
%% @doc Stop the mixi_staging server.
stop() ->
    application:stop(mixi_staging).
