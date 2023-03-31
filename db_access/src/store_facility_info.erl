%% @doc Hello World handler

-module(store_facility_info).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	#{<<"facility_id">>:=Facility_id, <<"city">>:=City} = jsx:decode(Data),
	package_tracker:store_facility_info(package_tracker, {Facility_id, City}),
	% io:format("here is the json object decoded: ~p", [jsx:decode(Data)]),	
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
        }, "[\"done\"]", Req0),
        {ok, Req, Opts}.

