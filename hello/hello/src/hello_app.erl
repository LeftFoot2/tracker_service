%%%-------------------------------------------------------------------
%% @doc hello public API
%% @end
%%%-------------------------------------------------------------------

-module(hello_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
            {'_', [
<<<<<<< HEAD
                {"/", default_page_h, []}
                
            ]}
        ]),
    	PrivDir = code:priv_dir(tracker_business_logic), 
    	%tls stands for transport layer security
          {ok,_} = cowboy:start_tls(https_listener, [
                  		{port, 443},
				{certfile, PrivDir ++ "/ssl/fullchain.pem"},
				{keyfile, PrivDir ++ "/ssl/privkey.pem"}
              		], #{env => #{dispatch => Dispatch}}),
=======
                {"/", default_page_h, []},
                {"/hello", hello_page_h, []}
            ]}
        ]),
        cowboy:start_clear(
            my_http_listener,
            [{port, 80}],
            #{env => #{dispatch => Dispatch}}
        ),
>>>>>>> f75f7c3cdc20e65c13ce827628aaebfd1e50e7d7
        hello_sup:start_link().

stop(_State) ->
    ok.                                                                                     

%% internal functions

%% internal functions
