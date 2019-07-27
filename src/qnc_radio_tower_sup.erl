%% @author efen
%% @since 27/07/2019
%% @doc
%% start radio tower processes
-module (qnc_radio_tower_sup).
-behaviour (supervisor).

-include ("../include/qnc_radio_tower_constants.hrl").


%% --------
%% API functions
%% --------
-export ([start/1, stop/0]).
-export ([init/1]).



%% ---------
%% internal functions
%% --------

%% @doc
%% start the supervisor
%% @spec (Args::term())-> {ok, pid()} | pid() | ok
start(Args)->	supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).


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
