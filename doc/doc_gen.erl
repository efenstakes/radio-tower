%% @author efen
%% @since 1/7/2018
%% doc
%% generate edoc
-module (doc_gen).

%%  ------
%% interface functions
%% ------
-export ([start/0]).


%% ------
%% internal functions
%% ------
start()->
	generate_documentation().

generate_documentation()->
	io:format("reading source files ~n"),
	Files = filelib:wildcard("../src/*.erl"),
	io:format("acquired source files ~n"),
	io:format("generating documentation ~n"),
	edoc:run(Files, ".").

