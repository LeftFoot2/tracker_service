-module(db_api).
-export([put_friends_for/3, put_package/3, get_package/2]).


put_friends_for(Name_key,Friends,Pid)->
	Request=riakc_obj:new(<<"friends">>, Name_key, Friends),
	riakc_pb_socket:put(Pid, Request).

put_package(Package_ID, Location_ID, Pid) ->
	Request=riakc_obj:new(<<"package">>, Package_ID, Location_ID),
	riakc_pb_socket:put(Pid, Request).

get_package(Package_ID, Pid) ->
	Request=riakc_obj:get_value(Package_ID),
	riakc_pb_socket:get(Pid, Request).