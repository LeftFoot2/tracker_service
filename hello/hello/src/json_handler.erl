-module(json_handler).

-export([init/2]).

init(Req0, Opts) ->
    {Method, Req1} = cowboy_req:method(Req0),
    io:format("Received request with method: ~p~n", [Method]),
    case Method of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req1),
            io:format("Received body: ~s~n", [Body]),
            try jsx:decode(Body, [return_maps]) of
                #{<<"name">> := Name} ->
                    io:format("Extracted name: ~p~n", [Name]),
                    Req3 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
                        Name,
                        Req2),
                    {ok, Req3, Opts}
            catch
                _:Error ->
                    io:format("Error decoding JSON or sending response: ~p~n", [Error]),
                    Req3 = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
                        "Internal Server Error",
                        Req2),
                    {ok, Req3, Opts}
            end;
        _ ->
            Req2 = cowboy_req:reply(405,
                #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
                "Method Not Allowed",
                Req1),
            {ok, Req2, Opts}
    end.
