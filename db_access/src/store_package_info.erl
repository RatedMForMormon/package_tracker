%% @doc Hello World handler

-module(store_package_info).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	#{<<"package_id">>:=Package_id, <<"holder_id">>:=Holder_id, <<"timestamp">>:=Timestamp} = jsx:decode(Data),
	package_tracker:store_package_info(package_tracker, {Package_id, Holder_id, Timestamp}),
	io:format("here is the json object decoded: ~p", [jsx:decode(Data)]),	
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
        }, "[\"done\"]", Req0),
        {ok, Req, Opts}.

