%% @author efen
%% @since 27/07/2019
%% @doc
%% start application supervisor processes
-module (qrt_sup).
-behaviour (supervisor).

-include ("../include/qrt_constants.hrl").


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
   SupSpecs = ?SUP_SPEC1,
   RadioSup = ?NEW_SUP(qnc_radio_sup, []),
   LogSup = ?NEW_CHILD(qrt_log, []),
   Childs = [ RadioSup, LogSup ],
   {ok, {SupSpecs, Childs}}.



%% @doc
%% stop the supervisor
%% @spec ()-> ok
stop()->
  case whereis(?MODULE) of
    undefined->  ok;
    Pid->  	     exit(Pid, shutdown)
  end.
