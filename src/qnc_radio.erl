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

-include ("../include/qrt_constants.hrl").
-include ("../include/qrt_schemas.hrl").


%% API functions
-export ([start/1, stop/1]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export ([ add_fan/2, remove_fan/2, clear_fans/1, get_fans/1 ]).

%% for tests
-export ([ get_next_timeout/1, get_sort_order/1 ]).


%% internal functions



%% client interface functions


%% start a radio 
start(RadioInfo)->   
   Name = RadioInfo#radio.name,
   gen_server:start_link({local, Name}, ?MODULE, RadioInfo, []).


%% stop a radio 
stop(Name)->   gen_server:cast(Name, shutdown).


%% add a radio fan
add_fan(Name, Fan)->   gen_server:cast(Name, { add_fan, Fan }).


%% remove a radio fan
remove_fan(Name, Fan)->   gen_server:cast(Name, { remove_fan, Fan }).


%% clear radio fan list
clear_fans(Name)->   gen_server:cast(Name, clear_fans).


%% get radio fans
get_fans(Name)->   gen_server:cast(Name, get_fans).




%% server centric functions


init(Args)->
   process_flag(trap_exit, true),
   % io:format("args ~p~n", [Args]),
   
   % Timeout = 

   {ok, Args, ?TIMEOUT}.


handle_call(_Req, From, State)->
   {reply, From, State, ?TIMEOUT}.


handle_cast(shutdown, State)->
   {stop, shutdown, State};

handle_cast(_Req, State)->
   {noreply, State, ?TIMEOUT}.



handle_info(timeout, State)->
   ThisTimeout = State#radio.next_timeout,

   case ThisTimeout of 

   	  clear_fans_after-> 
   	    case is_time_to_clear_fans(State) of 
   	    	true->    
   	    	   NewState = State#radio{ fans = [] },
   	    	   {noreply, NewState, ?TIMEOUT};
   	    	false->   
   	    	   {noreply, State, ?TIMEOUT}
   	    end;
      
      close_after->
        case is_close_time(State) of 
   	        true->   {stop, shutdown, State};
   	        false->  {noreply, State, ?TIMEOUT}
        end;
 
      check_emptiness when (State#radio.check_emptiness)#check_emptiness.afterr =/= undefined ->
        case is_check_emptiness_time(State) of 
        	true->
        	   case (State#radio.check_emptiness)#check_emptiness.action of 
        	   	 Actn when Actn == "CLOSE"->
        	   	    {stop, shutdown, State};
        	   	 _->
        	   	    {noreply, State, ?TIMEOUT}
        	   end;
        	false->
        	   {noreply, State, ?TIMEOUT}
        end;

       _->
        case is_close_time(State) of 
   	        true->   {stop, shutdown, State};
   	        false->  {noreply, State, ?TIMEOUT}
        end
                
   end;


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
get_next_timeout(State)-> 
   LastTimeoutEvent = State#radio.last_timeout,
   SortedTimeouts = get_sort_order(State),

   ListUntilLastTimeout = lists:takewhile(
   	                              fun(M)-> maps:get(name, M) == LastTimeoutEvent end, 
                                  SortedTimeouts
                          ),
   NextTimeoutIndex = length(ListUntilLastTimeout) + 1,
   NextTimeoutMap = lists:nth(NextTimeoutIndex, SortedTimeouts),
   maps:get(name, NextTimeoutMap).


get_sort_order(State)->
  ClearFansAfter = State#radio.clear_fans_after,
  CloseAfter = State#radio.close_after,
  CheckEmptinessAfter = State#radio.check_emptiness,
  
  ClearFansAfterMap = #{ time => ClearFansAfter#clear_fans_after.afterr, name => clear_fans_after, record => ClearFansAfter },
  CloseAfterMap = #{ time => CloseAfter, name => close_after, record => CloseAfter },
  CheckEmptinessAfterMap = #{ time => CheckEmptinessAfter#check_emptiness.afterr, name => check_emptiness, record => CheckEmptinessAfter },

  % filters out maps whose time key is undefined or 0
  NonNullFun = fun(M)-> maps:get(time, M) == undefined orelse maps:get(time, M) == 0 end,
  % get maps whose time key value is not undefined or 0
  Timeouts = lists:filter( NonNullFun, [ ClearFansAfterMap, CloseAfterMap, CheckEmptinessAfterMap ]),

  % sort the maps by time function
  Sorter = fun(TimeoutEvent_1, TimeoutEvent_2)->  maps:get(time, TimeoutEvent_1) > maps:get(time, TimeoutEvent_2) end,
  % sort the maps by time
  lists:usort(Sorter, Timeouts).


%% get next timeout event



%% @doc
%% check if radio expiry time is due 
is_close_time(State)-> 
   Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
   StartTime = State#radio.alive_since,
   ExpireAfter = State#radio.close_after,

   case (Now - StartTime) > ExpireAfter of 
   	  true->   true;
   	  _->      false
   end.



%% @doc
%% check if radio check emptiness time is due 
is_check_emptiness_time(State)-> 
   Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
   CheckAfter = (State#radio.check_emptiness)#check_emptiness.afterr,
   LastCheck = (State#radio.check_emptiness)#check_emptiness.last_check,

   case (Now - LastCheck) > CheckAfter of 
   	  true->   true;
   	  _->      false
   end.


%% @doc
%% check if its time to clear fans for the radio  
is_time_to_clear_fans(State)-> 
   Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
   ClearAfter = (State#radio.clear_fans_after)#clear_fans_after.afterr,
   LastCheck = (State#radio.clear_fans_after)#clear_fans_after.last_check,

   case (Now - LastCheck) > ClearAfter of 
   	  true->   true;
   	  _->      false
   end.
