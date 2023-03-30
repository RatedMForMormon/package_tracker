%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(package_tracker).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% for testing purposes

-export([store_package_info/2, query_package_history/2, store_vehicle_info/2, query_vehicle_history/2, store_facility_info/2, query_facility/2]).
-record(location, {lat=0.0, lon=0.0}).
-record(package, {package_id="", holder_id="", timestamp=0}).
-record(vehicle, {vehicle_id="", location=#location{lat=0.0, lon=0.0}, timestamp=0}).
-record(facility, {facility_id="", city=""}).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
%   io:fwrite("Hello, World!"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
%   io:fwrite("Hello, World"),
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
	case riakc_pb_socket:start_link("dev.cvm-labs.com", 8087) of
		{ok, Riak_pid} -> {ok, Riak_pid};
		_ -> {stop, link_failure}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
                        
% Store info functions
handle_call({store_package_info, {Package_id, Holder_id, Timestamp}}, _From, Riak_pid) ->
    case riakc_pb_socket:get(Riak_pid, <<"packages">>, term_to_binary(Package_id)) of
        {ok, Old_info} -> 
            Request=riakc_obj:new(<<"packages">>, term_to_binary(Package_id), term_to_binary([{Holder_id, Timestamp}] ++ binary_to_term(riakc_obj:get_value(Old_info))));
        _ -> 
            Request=riakc_obj:new(<<"packages">>, term_to_binary(Package_id), term_to_binary([{Holder_id, Timestamp}]))
    end,
    {reply, riakc_pb_socket:put(Riak_pid, Request), Riak_pid};

handle_call({store_vehicle_info, {Vehicle_id, {Lat, Long}, Timestamp}}, _From, Riak_pid) ->
    case riakc_pb_socket:get(Riak_pid, <<"vehicles">>, term_to_binary(Vehicle_id)) of
        {ok, Old_info} -> 
            Request = riakc_obj:new(<<"vehicles">>, term_to_binary(Vehicle_id), term_to_binary([{{Lat, Long}, Timestamp}] ++ binary_to_term(riak_obj:get_value(Old_info))));
        _ ->
            Request = riak_obj:new(<<"vehicles">>, term_to_binary(Vehicle_id), term_to_binary([{{Lat, Long}, Timestamp}]))
    end,
    {reply, riakc_pb_socket:put(Riak_pid, Request), Riak_pid};

handle_call({store_facility_info, {Facility_id, City}}, _From, Riak_pid) ->    
    Request = riakc_obj:new(<<"facilities">>, term_to_binary(Facility_id), term_to_binary(City)),
    {reply, riakc_pb_socket:put(Riak_pid, Request), Riak_pid};

% Get info functions
handle_call({query_package_history, Package_id}, _from, Riak_pid) ->
    {reply, riakc_pb_socket:get(Riak_pid, <<"packages">>, term_to_binary(Package_id)), Riak_pid};

handle_call({query_package_history, Vehicle_id}, _from, Riak_pid) ->
    {reply, riakc_pb_socket:get(Riak_pid, <<"vehicles">>, term_to_binary(Vehicle_id)), Riak_pid};

handle_call({query_facility, Facility_id}, _from, Riak_pid) ->
    {reply, riakc_pb_socket:get(Riak_pid, <<"facilities">>, term_to_binary(Facility_id)), Riak_pid};

% Shutdown server
handle_call(stop, _From, _State) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================


% Package functions
store_package_info(Registered_name, Decoded_object) ->
    gen_server:call(Registered_name, {store_package_info, Decoded_object}).

query_package_history(Registered_name, Package_id) ->
    gen_server:call(Registered_name, {query_package_history, Package_id}).

% Vehicle functions
store_vehicle_info(Registered_name, Decoded_object) ->
    gen_server:call(Registered_name, {store_vehicle_info, Decoded_object}).

query_vehicle_history(Registered_name, Vehicle_id) ->
    gen_server:call(Registered_name, {query_vehicle_history, Vehicle_id}).

% Facility functions
store_facility_info(Registered_name, Decoded_object) ->
    gen_server:call(Registered_name, {store_facility_info, Decoded_object}).

query_facility(Registered_name, Facility_id) ->
    gen_server:call(Registered_name, {query_facility, Facility_id}). 
% cases for no information
% bad ids
% bad timestamp
% good case


-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
-include_lib("eunit/include/eunit.hrl").

store_package_info_test_() ->
    {setup,
     fun() ->
             meck:new(riakc_obj),
             meck:new(riakc_pb_socket),
             meck:expect(riakc_obj, new, fun(Bucket, Key, Value) -> done end),
             meck:expect(riakc_pb_socket, put, fun(Riak_pid, Request) -> worked end)
     end,
     fun(_) ->
             meck:unload(riakc_obj),
             meck:unload(riakc_pb_socket)
     end,
     [
        ?_assertEqual({reply, worked, riak_pid},
                        package_tracker:handle_call({store_package_info, #package{package_id="12345-678-90", holder_id="09876-543-21", timestamp=10}}, from_pid, riak_pid)),

        ?_assertEqual({reply, fail, riak_pid},
                        package_tracker:handle_call({store_package_info, #package{package_id="12345-6789-0", holder_id="098765-43-21", timestamp=-20000000}}, from_pid, riak_pid)),

        ?_assertException(error, function_clause,
                        package_tracker:handle_call({store_package_info, {"12345", "54321", 12345}}, from_pid, riak_pid))
     ]}.

store_vehicle_info_test_() ->
    {setup,
     fun() -> 
             meck:new(riakc_obj),
             meck:new(riakc_pb_socket),
             meck:expect(riakc_obj, new, fun(Bucket, Key, Value) -> done end),
             meck:expect(riakc_pb_socket, put, fun(Riak_pid, Request) -> worked end)
     end,
     fun(_) ->
             meck:unload(riakc_obj),
             meck:unload(riakc_pb_socket)
     end,
     [
        ?_assertEqual({reply, worked, riak_pid},
                      package_tracker:handle_call({store_vehicle_info, #vehicle{vehicle_id="09876-543-21", location=#location{lat=50, lon=50}, timestamp=0}}, from_pid, riak_pid)),

        ?_assertEqual({reply, worked, riak_pid},
                      package_tracker:handle_call({store_vehicle_info, #vehicle{vehicle_id="09876-543-21", location=#location{lat=50, lon=50}, timestamp=0}}, from_pid, riak_pid)),

        ?_assertEqual({reply, worked, riak_pid},
                      package_tracker:handle_call({store_vehicle_info, #vehicle{vehicle_id="09876-543-21", location=#location{lat=50, lon=50}, timestamp=0}}, from_pid, riak_pid))

     ]}.

store_facility_test() ->
    {setup,
     fun() -> 
             meck:new(riakc_obj),
             meck:new(riakc_pb_socket),
             meck:expect(riakc_obj, new, fun(Bucket, Key, Value) -> done end),
             meck:expect(riakc_pb_socket, put, fun(Riak_pid, Request) -> worked end)

     end,
     fun(_) ->
             meck:unload(riakc_obj),
             meck:unload(riakc_pb_socket)
     end,
     [
        ?_assertEqual({reply, worked, riak_pid},
                      package_tracker:handle_call({store_facility, #facility{facility_id="01234-567-89", city="Rexburg, ID"}}, from_pid, riak_pid)),

        ?_assertEqual({reply, worked, riak_pid},
                      package_tracker:handle_call({store_facility, #facility{facility_id="01234-567-89", city="Rexburg, ID"}}, from_pid, riak_pid)),

        ?_assertEqual({reply, worked, riak_pid},
                      package_tracker:handle_call({store_facility, #facility{facility_id="01234-567-89", city="Rexburg, ID"}}, from_pid, riak_pid))

     ]}.

query_package_history_test() ->
    {setup, 
     fun() -> 
             meck:new(riakc_pb_socket),
             meck:expect(riakc_pb_socket, search, fun(Riak_pid, Bucket, Key) -> [meep] end)
     end,
     fun(_) ->
             meck:unload(riakc_pb_socket)
     end,
     [
      ?_assertEqual([meep], 
                    package_tracker:handle_call({query_package_history, "12345-678-90"}, from_pid, riak_pid))
     ]}.

query_vehicle_history_test() ->
    {setup,
     fun() -> 
             meck:new(riakc_pb_socket),
             meck:expect(riakc_pb_socket, search, fun(Riak_pid, Bucket, Key) -> [{#location{lat=50, lon=50}, 90}] end)
     end,
     fun(_) ->
             meck:unload(riakc_pb_socket)
     end,
     [
      ?_assertEqual([{#location{lat=50, lon=50}, 90}],
                    package_tracker:handle_call({query_vehicle_info, "09876-543-21"}, from_pid, riak_pid))
     ]}.

query_facility_test() ->
    {setup,
     fun() -> 
             meck:new(riakc_pb_socket),
             meck:expect(riakc_pb_socket, search, fun(Riak_pid, Bucket, Key) -> "Houston" end)
     end,
     fun(_) ->
             meck:unload(riakc_pb_socket)
     end,
     [
      ?_assertEqual("Houston", 
                    package_tracker:handle_call({query_facility, "01234-567-89"}, from_pid, riak_pid))
     ]}.

-endif.
