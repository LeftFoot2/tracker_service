-module(json_handler).

-export([init/2]).

init(Req0, Opts) ->
    {Method, Req1} = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req1),
            try jsx:decode(Body, [return_maps]) of
                #{<<"name">> := Name} ->
                    % Convert binary "Name" to list for response (if necessary)
                    Req3 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
                        Name,  % Send back the name directly
                        Req2),
                    {ok, Req3, Opts}
            catch
                _:_ ->
                    Req3 = cowboy_req:reply(400,
                        #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
                        "Invalid JSON data.",
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
