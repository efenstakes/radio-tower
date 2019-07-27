%% @author efen
%% @first writing 27/07/2019
%% @todo
%% keep the app constants


%% time constants
%% --
-define (O_TIME, 0).
-define (ZERO, ?O_TIME).
-define (O_SEC, 1000).
-define (O_MIN, (?O_SEC*60)).
-define (O_HOUR, (?O_MIN*60)).
-define (O_DAY, (?O_HOUR*24)).
-define (O_WEEK, (?O_DAY*7)).
-define (O_MONTH, (?O_WEEK*4)).
-define (O_YEAR, (?O_MONTH*12)).
-define (O_DECADE, (?O_YEAR*10)).
-define (O_CENTURY, (?O_DECADE*10)).

%% other constants
%% --
-define (DB_TIMEOUT, (?O_SEC*5)).
-define (TIMEOUT, (?O_SEC*5)).

%% supervisor related macros
%% --
%-define (CHILD (NAME), {NAME, {NAME, start, []}}).

-define (DOWN (Param), io:format("already down ~p~n", [Param])).
-define (STOPPING (), io:format("server shutting down normally~n")).
-define (STOPPING (Reason, State), io:format("server stopping cause of ~p with reason ~p~n", [Reason, State])).




%% supervisor specification macros
-define (NEW_KID(Id, Name, Args, RestartType, ShutdownTime, KIDTYPE), 
	             {Id, {Name, start, Args}, RestartType, ShutdownTime, KIDTYPE, [Id]
	    }).

-define (NEW_CHILD(Id, Name, Args, RestartType, ShutdownTime), ?NEW_KID(Id, Name, Args, permanent, ShutdownTime, worker) ).
-define (NEW_CHILD(Name, Args), ?NEW_KID(Name, Name, Args, permanent, ?MAX_SHUTDOWN_TIME, worker) ).
-define (NEW_SUP(Name, Args, ShutdownTime), ?NEW_KID(Name, Name, Args, permanent, ShutdownTime, supervisor) ).
-define (NEW_SUP(Name, Args), ?NEW_KID(Name, Name, Args, permanent, ?SUP_MAX_SHUTDOWN_TIME, supervisor) ).


-define (SUP_MAX_RESTARTS, 4).
-define (SUP_MAX_RESTART_TIME, (?O_MIN*10)).
-define (SUP_MAX_SHUTDOWN_TIME, (?O_MIN*5)).

-define (SUP_SPEC1, {one_for_one, ?SUP_MAX_RESTARTS, ?SUP_MAX_RESTART_TIME}).
-define (SUP_SPEC2, {rest_for_one, ?SUP_MAX_RESTARTS, ?SUP_MAX_RESTART_TIME}).
-define (SUP_SPEC3, {simple_one_for_one, ?SUP_MAX_RESTARTS, ?SUP_MAX_RESTART_TIME}).


%% child related
-define (MAX_RESTARTS, 10).
-define (MAX_RESTART_TIME, (?O_MIN*2)).
-define (MAX_SHUTDOWN_TIME, (?O_MIN)).

