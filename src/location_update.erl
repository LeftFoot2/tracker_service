-module(location_update).

-export([init/2]).

init(Req0, Opts) ->
        {ok,Data,_} = cowboy_req:read_body(Req0),
        [{<<"Location_ID">>, Location_ID},
        {<<"Coordinates">>, [{<<"Latitude">>, Latitude}, {<<"Longitude">>, Longitude}]}] = jsx:decode(Data),
        Result = erpc:call('deliveryman@bl.thomasjamiesonprograms.com', business_logic, put_location, [Location_ID,{Latitude,Longitude}]), 
        
        Encoded_message = jsx:encode(Result),


        Response = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/json">>},
                Encoded_message, Req0),
        {ok, Response, Opts}.




