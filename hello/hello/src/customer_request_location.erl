-module(customer_request_location).

-export([init/2]).

init(Req0, Opts) ->
    {ok,Data,_} = cowboy_req:read_body(Req0),
    % it is expected that the data consists of one quoted-string name
    % in an array.
    % Req = cowboy_req:reply(200, 
    %         #{<<"content-type">> => <<"text/plain">>},
    %         "Hello world this is exciting! Test", Req0),
    % {ok, Req, Opts},
    
    ["package_1"]  = jsx:decode(Data),
    [Package_Id]  = jsx:decode(Data),
    % test [<<"35">>,<<"14">>] = [Package_ID,Location_ID],
    Result = erpc:call('deliveryman@bl.thomasjamiesonprograms.com', business_logic, location_request, ["Package_ID"]),
    
    %get_friends_server:get_friends_of(Package_ID) of
            %         {error,notfound} -> "no such person";
            %         Friends -> Friends
            % end,
        {ok,Lat_Long} = Result,
    Encoded_message = jsx:encode(Lat_Long),
    %io:format("~p~n",[get_friends_server:get_friends_of(Name)]),

    Response = cowboy_req:reply(200,
            #{<<"content-type">> => <<"text/json">>},
            Encoded_message, Req0),
    {ok, Response, Opts}.

