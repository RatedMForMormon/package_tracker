%% @doc Hello World handler

-module(store_package_info).

-export([init/2]).

init(Req0, Opts) ->
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
        }, "[\"Hello world!\"]", Req0),
        {ok, Req, Opts}.

