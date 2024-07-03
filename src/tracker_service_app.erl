%%%-------------------------------------------------------------------
%% @doc tracker_service public API
%% @end
%%%-------------------------------------------------------------------

-module(tracker_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    
        Dispatch = cowboy_router:compile([
            {'_', [
                {"/", default_page_h, []},
                {"/package_transferred", package_transfer_page, []},
                {"/delivered", delivered_handler, []},
                {"/location_request", customer_request_location, []},
                {"/location_update", location_update, []}    
            ]}
        ]),

    	PrivDir = code:priv_dir(tracker_service),
    	%tls stands for transport layer security
          {ok,_} = cowboy:start_tls(https_listener, [
                  		{port, 443},
				{certfile, PrivDir ++ "/ssl/fullchain.pem"},
				{keyfile, PrivDir ++ "/ssl/privkey.pem"}
              		], #{env => #{dispatch => Dispatch}}),

        tracker_service_sup:start_link().

stop(_State) ->
    ok.                                                                                     

%% internal functions

%% internal functions
