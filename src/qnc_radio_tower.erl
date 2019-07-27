%% @author efen 
%% @since dd/mm/yy - 27/07/2019
%% doc
%% a pubsub server
%% 
-module (qnc_radio_tower).
-behavior (application).


%% -----
%% API functions
%% -----


%% @@@ application behavior function
-export ([start/0, start/2, start_phase/3]).
-export ([prep_stop/1, stop/1]).


%% @@@ application api




%% -----
%% internal functions
%% -----


%% @doc
%% start application
%% @spec ()-> {ok, pid()} | ok
start()->	start(normal, []).


%% @doc
%% start application
%% @spec (_StartType::atom(), StartArgs::term())-> {ok, pid()} | ok
start(_StartType, StartArgs)->	qnc_radio_tower_sup:start(StartArgs).



%% @doc
%% phases 
start_phase(phase_one, _StartType, _Args)-> io:format("phase one ~n"), ok.



%% @doc
%% stop any www processes and applications before the application processes can be stopped
%% this will allow stopping of request receprion before processes are shutdown
prep_stop(_)->  application:stop(ranch), ok.



%% @doc
%% stop application
%% @@could call init:stop() to clean env and stop the erlang runtime
%% @spec (_Args::term())-> {ok, pid()} | ok
stop(_Args)->	ok.
