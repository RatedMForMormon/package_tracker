-module(query_vehicle_history).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	#{<<"vehicle_id">> := Vehicle_id} = jsx:decode(Data),
	{ok, Response} = package_tracker:query_vehicle_history(package_tracker, Vehicle_id),
	io:format("Riak Content: ~p~n", [binary_to_term(riakc_obj:get_value(Response))]),
	Decoded = jsx:encode(#{history => binary_to_term(riakc_obj:get_value(Response))}),
		    %binary_to_term(riakc_obj:get_value(Response))),
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/json">>
        }, Decoded, Req0),
        {ok, Req, Opts}.

