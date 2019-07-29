%% @author efen
%% @since 27/07/2019
%% @doc
%% start radio tower processes
-module (qnc_radio_sup).
-behaviour (supervisor).


-include ("../include/qrt_constants.hrl").
-include ("../include/qrt_schemas.hrl").


%% --------
%% API functions
%% --------
-export ([start/0, stop/0]).
-export ([init/1]).

-export ([ new_radio/1, close_radio/1 ]).


%% ---------
%% internal functions
%% --------

%% @doc
%% start the supervisor
%% @spec ()-> {ok, pid()} | pid() | ok
start()->	supervisor:start_link({local, ?MODULE}, ?MODULE, [ ?MODULE ]).


%% @doc
%% start supervisor children
%% @spec (Args::term())-> {ok, pid()} | pid() | ok
init(_Args)->
   SupSpecs = ?SUP_SPEC3,
   RadioTower = ?NEW_CHILD(qnc_radio_tower, []),
   Childs = [ RadioTower ],
   {ok, {SupSpecs, Childs}}.



%% @doc
%% stop the supervisor
%% @spec ()-> ok
stop()->
  case whereis(?MODULE) of
    undefined->  ok;
    Pid->  	     exit(Pid, shutdown)
  end.



%% @doc
%% start supervisor children
%% @spec (Args::term())-> {ok, pid()} | pid() | ok
new_radio(Args)->
   case whereis(?MODULE) of
    Pid when is_pid(Pid)->
   	   supervisor:start_child(?MODULE, [Args]);
   	undefined->
   	    ok
   end.


close_radio(RadioName)->
   qnc_radio:stop(RadioName).
   % supervisor:terminate_child(?MODULE, RadioName).
