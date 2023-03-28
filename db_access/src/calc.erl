-module(calc).
-export([add/2]).

add(Number1, Number2) ->
	Number1 + Number2.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
add_test() ->
	{setup,
	 fun() -> meck:load(calc),
		  meck:expect(calc, add, fun(4, 4) -> 8 end)
	 end,
	 fun() -> meck:unload(calc) end,
	 [
	  ?assertEqual(8, calc:add(4,4)),
	  ?assertEqual(8.5, calc:add(4.0, 4.5))
	 ]}.
-endif.
