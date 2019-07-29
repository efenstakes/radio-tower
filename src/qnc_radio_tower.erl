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

-export ([ new_radio/1, close_radio/1, get_radio_list/0 ]).

-export ([ add_fan/2, remove_fan/2, clear_fans/1, get_fans/1 ]).




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
start(_StartType, StartArgs)->	qrt_sup:start(StartArgs).



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






%% @doc
%% create a new radio station
%% @spec (Params::#radio{})-> {ok, pid()} | ok
new_radio(Params)->   qnc_radio_sup:new_radio(Params).

%% @doc
%% close a radio station given the radio's name
%% @spec (Params::term())-> ok
close_radio(Params)->   qnc_radio_sup:close_radio(Params).

%% @doc
%% get the list of radio stations
%% @spec ()-> []
get_radio_list()->   [].



%% @doc
%% add a new radio station fan
%% @spec (Radio::term(), Fan::term())-> ok
add_fan(Radio, Fan)->   qnc_radio:add_fan(Radio, Fan).

%% @doc
%% remove radio station fan
%% @spec (Radio::term(), Fan::term())-> ok
remove_fan(Radio, Fan)->   qnc_radio:remove_fan(Radio, Fan).

%% @doc
%% clear a radio station fan list
%% @spec (Radio::term())-> ok
clear_fans(Radio)->   qnc_radio:clear_fans(Radio).

%% @doc
%% get radio station fan list
%% @spec (Radio::term())-> ok
get_fans(Radio)->   qnc_radio:get_fans(Radio).
