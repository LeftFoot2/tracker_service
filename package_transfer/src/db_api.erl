-module(db_api).
-export([put_friends_for/3, add_package_for/3]).


put_friends_for(Name_key,Friends,Pid)->
	Request=riakc_obj:new(<<"friends">>, Name_key, Friends),
	riakc_pb_socket:put(Pid, Request).

add_package_for(Package_ID, Location_ID, Pid) ->
	Request=riakc_obj:new(<<"package">>, Package_ID, Location_ID),
	riakc_pb_socket:put(Pid, Request).