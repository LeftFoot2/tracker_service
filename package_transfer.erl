%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license/deliveryman"
%%        href="http://creativecommons.org/licenses/by/4.0/p1!"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%

-module(package_transfer).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0,add_package/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================



%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    Starting_packages = [{"4","101"},{"5","301"}],
    gen_server:start_link({local, package_transfer}, ?MODULE, Starting_packages, []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

add_package(Package_ID, Location_ID) ->
    gen_server:cast(?MODULE, {add_pckg, Package_ID, Location_ID}).




%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init(List) ->
        Map = lists:foldl(fun({Key, Value}, AccMap) -> maps:put(Key, Value, AccMap) end, #{}, List),
        {ok, Map}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
handle_call(stop, _From, _State) ->
        {stop,normal,replace_stopped,down}. %% setting the server's internal state to down
% handle_call(Request, From, State) ->
%         {reply,replace_started,State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.

handle_cast({add_pckg, Package_ID, Location_ID}, State) ->
    NewState = maps:put(Package_ID, Location_ID, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================



-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
-include_lib("eunit/include/eunit.hrl").


test_transfer() ->
    {setup,
     fun() -> %this setup fun is run once befor the tests are run. If you want setup and teardown to run for each test, change {setup to {foreach
        meck:new(db_api),
        meck:expect(db_api, package_transfer, fun(Key,Names,PID) -> worked end)
        
     end,
     fun(_) ->%This is the teardown fun. Notice it takes one, ignored in this example, parameter.
        meck:unload(db_api)
     end,
    [%This is the list of tests to be generated and run.
        ?_assertEqual({noreply, NewState},
                            transfer_test:handle_call({package_transfer,<<"3">>,[]}, some_from_pid, some_Db_PID))
    ]}.

%%
-endif.