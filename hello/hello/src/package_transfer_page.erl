%% @doc package transfer handler.
-module(package_transfer_page).

-export([init/2]).

% init(Req0, Opts) ->
%         Req = cowboy_req:reply(200, #{
%                 <<"content-type">> => <<"text/plain">>
%         }, "Hello world this is exciting! Test", Req0),
%         {ok, Req, Opts}.
init(Req0, Opts) ->
        {ok,Data,_} = cowboy_req:read_body(Req0),
                %it is expected that the data consists of one quoted-string name
                %in an array.
                % Req = cowboy_req:reply(200, #{
                %              <<"content-type">> => <<"text/plain">>
                %      }, "Hello world this is exciting! Test", Req0),
                %      {ok, Req, Opts},

                [Package_ID,Location_ID] = jsx:decode(Data),
                Result = erpc:call('deliveryman@bl.thomasjamiesonprograms.com', business_logic, package_transfer_url_handler, [Package_ID,Location_ID]), 
        %get_friends_server:get_friends_of(Package_ID) of
                        %         {error,notfound} -> "no such person";
                        %         Friends -> Friends
                        % end,
                        
                Encoded_message = jsx:encode(Result),
                %io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
                Response = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"text/json">>
                }, Encoded_message, Req0),
                {ok, Response, Opts}.
