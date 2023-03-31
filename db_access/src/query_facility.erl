-module(query_facility).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	#{<<"facility_id">> := Facility_id} = jsx:decode(Data),
	{ok, Response} = package_tracker:query_facility(package_tracker, Facility_id),
	io:format("Riak Content: ~p~n", [binary_to_term(riakc_obj:get_value(Response))]),
	Decoded = jsx:encode(binary_to_term(riakc_obj:get_value(Response))),
		    %binary_to_term(riakc_obj:get_value(Response))),
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/json">>
        }, Decoded, Req0),
        {ok, Req, Opts}.

