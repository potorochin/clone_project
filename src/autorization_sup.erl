%%%-------------------------------------------------------------------
%% @doc quick_pay top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(autorization_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	

    inets:start(),  
    ssl:start(), 
    httpc:set_options([{proxy, {{"proxy.pbank.com.ua", 8080}, ["localhost"]}}]),
	io:format("~n~nSupervisor Start~n~n"),

	SupSpec =  {          % Global supervisor options
      one_for_all,        % - use the one-for-one restart strategy
      100,               % - and allow a maximum of 1000 restarts
      300                % - per hour for each child process
    },

     ChildSpec = [         % The list of child processes you should supervise
        {   
        	aceptor,     % - Register it under the name hello_server
	        {                 % - Here's how to find and start this child's code 
		        aceptor,   %   * the module is called hello_server
		        start_link,     %   * the function to invoke is called start_link
		        []              %   * and here's the list of default parameters to use
	        },                
		        permanent,        % - child should run permantenly, restart on crash 
		        2000,             % - give child 2 sec to clean up on system stop, then kill 
		        worker,           % - FYI, this child is a worker, not a supervisor
		        [aceptor]    % - these are the modules the process uses  
	      	}],



    {ok, {SupSpec, ChildSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================
