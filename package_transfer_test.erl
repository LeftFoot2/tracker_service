

-module(package_transfer_test).
-import(package_transfer,[start/0,start/3,stop/0,add_package/2]).
%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("meck/include/meck.hrl").


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