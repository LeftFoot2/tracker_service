

-module(package_transfer_test).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



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