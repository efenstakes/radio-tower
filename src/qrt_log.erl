%% @author efen
%% @first writing 27/07/2019
%% @todo
%% radio fans process which keeps a list of radio subscribers
%% add, delete, clear fans
%% state is a list of fans
-module (qrt_log).
-behaviour (gen_server).

-include ("../include/qrt_constants.hrl").


%% API functions
-export ([start/0, stop/0]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export ([ add/1, remove/1, get_all/0 ]). 

%% internal functions


%% client interface functions

start()->    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop()->   gen_server:cast(?MODULE, shutdown).


%% @doc
%% add a new log
add(Log)->    gen_server:cast(?MODULE, { add_log, Log }).

%% @doc
%% remove a log
remove(Log)->    gen_server:cast(?MODULE, { remove_log, Log }).

%% @doc
%% get all logs
get_all()->    gen_server:cast(?MODULE, get_all).



%% server centric functions


init(Args)->
   process_flag(trap_exit, true),
   % io:format("args ~p~n", [Args]),
   {ok, Args}.



handle_call(get_all, _From, State)->
   {reply, State, State};

handle_call(_Req, _From, State)->
   {reply, _From, State}.



handle_cast({ add_log, Log }, State)->
   NewLogList = [ term_to_binary(Log), State ],
   {noreply, NewLogList};

handle_cast({ remove_log, Log }, State)->
   BinaryLogName = term_to_binary(Log),
   NewLogList = lists:filter( fun(L)-> L == BinaryLogName end, State),
   {noreply, NewLogList};

handle_cast(shutdown, State)->
   {stop, shutdown, State};

handle_cast(_Req, State)->
   {noreply, State}.



handle_info(timeout, Req)->
   {stop, shutdown, Req};
   % {noreply, NewReq, ?MECH_TIMEOUT};
handle_info({'EXIT', _Pid, _Reason}, State) -> 
   % io:format("process ~p terminated with Reason ~p'~n",[Pid, Reason]),
   {noreply, State};
handle_info(_Info, State)->
   % io:format("processin unknown message.. ~p~n", [_Info]),
   {noreply, State}.


code_change(_OldVsn, State, _Extra)->
   {noreply, State}.


terminate(Reason, State) when Reason == shutdown; Reason == normal->
   % io:format("shutting down normally with state ~p~n", [State]),
   State;
terminate(_Reason, State)->
   % io:format("shutting down abdormally cause of ~p with state ~p~n", [Reason, State]),
   State.

