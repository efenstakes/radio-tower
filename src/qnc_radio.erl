%% @author efen
%% @first writing 27/07/2019
%% @todo
%% radio process which keeps a subscription topic
%% add, delete, clear fans
%% state is a map with 
%% clear_after -- time after which to clear fans
%% expire_after -- time after which to kill the process
%% check_emptiness -- map -- action, after, last_check
%%       after how long should the radio check if it still has fans and the action to take - EXPIRE/LIVE_ON
%% last_check -- map -- clear_after/expire_after/check_emptiness
%% next_timeout -- the next timeout
%% fans -- the radio subscribers

%% because of many timeouts, the radio timeout will be set to the least and then after the least set to the
%% 2nd least - least and so on
-module (qnc_radio).
-behaviour (gen_server).

-include ("../include/qnc_radio_tower_constants.hrl").


%% API functions
-export ([start/1, stop/1]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% internal functions



%% client interface functions

start(Req)->   gen_server:start_link({local, Req#req.id}, ?MODULE, Req, []).


stop(Name)->   gen_server:cast(Name, shutdown).



%% server centric functions


init(Args)->
   process_flag(trap_exit, true),
   % io:format("args ~p~n", [Args]),
   {ok, Args, ?TIMEOUT}.


handle_call(_Req, From, State)->
   {reply, From, State, ?TIMEOUT}.


handle_cast(shutdown, State)->
   {stop, shutdown, State};

handle_cast(_Req, State)->
   {noreply, State, ?TIMEOUT}.



handle_info(timeout, Req)->
   {stop, shutdown, Req};
   % {noreply, NewReq, ?MECH_TIMEOUT};
handle_info({'EXIT', _Pid, _Reason}, State) -> 
   % io:format("process ~p terminated with Reason ~p'~n",[Pid, Reason]),
   {noreply, State};
handle_info(_Info, State)->
   % io:format("processin unknown message.. ~p~n", [_Info]),
   {noreply, State, ?TIMEOUT}.


code_change(_OldVsn, State, _Extra)->
   {noreply, State, ?TIMEOUT}.


terminate(Reason, State) when Reason == shutdown; Reason == normal->
   % io:format("shutting down normally with state ~p~n", [State]),
   State;
terminate(_Reason, State)->
   % io:format("shutting down abdormally cause of ~p with state ~p~n", [Reason, State]),
   State.



%% @doc
%% calculate the next timeout given the last event and state
get_next_timeout(LastEvent, State)-> State.
