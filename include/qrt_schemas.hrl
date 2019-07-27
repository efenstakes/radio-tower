

%% check_emptiness -- action, after, last_check
%% after how long should the radio check if it still has fans and the action to take - CLOSE/LIVE_ON
%% when was the last check
-record (check_emptiness, { 
	         action, 
	         afterr, 
	         last_check 
	    }).

-record (clear_fans_after, { 
	         afterr, 
	         last_check 
	    }).


%% keep radio state
%% clear_fans_after -- time after which to clear fans
%% close_after -- time after which to kill the process
%% last_timeout  -- clear_fans_after/close_after/check_emptiness
%% next_timeout -- the next timeout -- clear_fans_after/close_after/check_emptiness
%% fans -- the radio subscribers
-record (radio, {
	             alive_since = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
                 clear_fans_after = #clear_fans_after{}, 
                 close_after,
                 check_emptiness = #check_emptiness{},
                 last_timeout,
                 next_timeout,
                 fans = [],
                 name
	}).