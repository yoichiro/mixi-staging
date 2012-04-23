%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mixi_staging Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mixi_staging application.

-module(mixi_staging_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mixi_staging.
start(_Type, _StartArgs) ->
    mixi_staging_deps:ensure(),
    mixi_staging_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mixi_staging.
stop(_State) ->
    ok.
