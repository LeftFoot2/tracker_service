-module(json_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            JsonData = jsx:decode(Body, [return_maps]),
            Name = maps:get(<<"name">>, JsonData, <<"">>),
            Email = maps:get(<<"email">>, JsonData, <<"">>),
            Status = maps:get(<<"status">>, JsonData, <<"">>),
            Message = io_lib:format("My name is ~s and my email is ~s, I am an ~s user.", [Name, Email, Status]),
            Req2 = cowboy_req:reply(200,
                #{
                    <<"content-type">> => <<"text/plain">>
                },
                Message,
                Req1),
            {ok, Req2, Opts};
        _ ->
            Req1 = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0),
            {ok, Req1, Opts}
    end.
