-module(db_api).
-export([put_package/3, get_package/2]).


put_package(Package_ID, Location_ID, Pid) ->
	Request=riakc_obj:new(<<"package">>, Package_ID, Location_ID),
	riakc_pb_socket:put(Pid, Request).

get_package(Package_ID, Pid) ->
	Request=riakc_obj:get_value(Package_ID),
	riakc_pb_socket:get(Pid, Request).