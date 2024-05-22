-module(db_api).
-export([put_friends_for/3]).


put_friends_for(Name_key,Friends,Pid)->
	Request=riakc_obj:new(<<"friends">>, Name_key, Friends),
	riakc_pb_socket:put(Pid, Request).