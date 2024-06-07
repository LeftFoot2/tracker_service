%% @JSON Test handler
-module(json_handler).

-export([init/2]).

init(Req0, Opts) ->
    %% Check if the method is POST
    {Method, Req1} = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            %% Read the body, which is assumed to be JSON
            {ok, Body, Req2} = cowboy_req:read_body(Req1),
            %% Decode JSON body
            JsonData = jsx:decode(Body, [return_maps]),
            %% Generate the response based on JSON content
            Name = maps:get(<<"name">>, JsonData, <<"">>),
            Email = maps:get(<<"email">>, JsonData, <<"">>),
            Status = maps:get(<<"status">>, JsonData, <<"">>),
            Message = io_lib:format("My name is ~s and my email is ~s, I am an ~s user.", [Name, Email, Status]),
            %% Send the response
            Req3 = cowboy_req:reply(200,
                #{
                    <<"content-type">> => <<"text/plain">>
                },
                Message,
                Req2),
            {ok, Req3, Opts};
        _ ->
            %% If not a POST request, return method not allowed
            Req3 = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req1),
            {ok, Req3, Opts}
    end.
