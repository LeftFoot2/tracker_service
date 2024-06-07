%% @doc Hello world handler.
-module(package_transfer_page).

-export([init/2]).

init(Req0, Opts) ->
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
        }, "Hello world this is exciting! Test", Req0),
        {ok, Req, Opts}.