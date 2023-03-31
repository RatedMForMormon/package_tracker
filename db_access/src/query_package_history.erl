-module(query_package_history).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	#{<<"package_id">> := Package_id} = jsx:decode(Data),
	{ok, Response} = package_tracker:query_package_history(package_tracker, Package_id),
	io:format("Riak Content: ~p~n", [binary_to_term(riakc_obj:get_value(Response))]),
	Decoded = jsx:encode(#{history => binary_to_term(riakc_obj:get_value(Response))}),
		    %binary_to_term(riakc_obj:get_value(Response))),
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/json">>
        }, Decoded, Req0),
        {ok, Req, Opts}.

