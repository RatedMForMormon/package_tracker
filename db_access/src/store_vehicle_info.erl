%% @doc Hello World handler

-module(store_vehicle_info).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	#{<<"vehicle_id">> := Vehicle_id, <<"location">> := #{<<"lat">> := Lat, <<"long">> := Long}, <<"timestamp">>:=Timestamp} = jsx:decode(Data),
	package_tracker:store_package_info(package_tracker, {Vehicle_id, {Lat, Long}, Timestamp}),
	% io:format("here is the json object decoded: ~p", [jsx:decode(Data)]),	
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
        }, "[\"done\"]", Req0),
        {ok, Req, Opts}.

