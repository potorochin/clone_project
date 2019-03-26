
-module(aceptor). 
-behaviour(gen_server).   
-define(SERVER, ?MODULE). 


-export([start_link/0, getMe/0, sendMessage/0, forwardMessage/0,getUpdates/0]).


-export([                      
  init/1,                      
  handle_call/3,               
  handle_cast/2,               
  handle_info/2,               
  terminate/2,                 
  code_change/3]).             

start_link() ->          
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-define(HOST,   "https://api.telegram.org/bot").
-define(TOKEN,  "702014129:AAEmJdQQhEm8oGrh0Ai3szKUCQqVul-T2qE").
-define(GETME,  "/getUpdates").
-define(GETUPDATES,  "/getUpdates").
-define(SENDMESSAGE,  "/sendMessage").
-define(FORWARDMESSAGE,  "/forwardMessage").


getMe() ->
  gen_server:cast(?MODULE, get_me).

sendMessage() ->
  gen_server:cast(?MODULE, send_message).

forwardMessage() ->
  gen_server:cast(?MODULE, forward_message).

getUpdates() ->
  gen_server:cast(?MODULE, getUpdates).

init([]) ->  
  io:format("~n~nAceptor Start~n~n"),     

    {ok, []}.     


handle_call(_Msg, _From, State) -> 


    {reply, _From, State}.


handle_cast(Msg, State) ->   
    Method = case Msg of
      get_me -> ?GETME;
      send_message -> ?SENDMESSAGE;
      forward_message -> ?FORWARDMESSAGE;
      getUpdates -> ?GETUPDATES;
      _ -> io:format("Unknown Comand"), <<"">>
    end,

    Url = ?HOST ++ ?TOKEN ++ Method,
    %Body = <<"{\"chat_id\":\"93906430\",\"text\":\"93906430\"}">>,
    Body = <<"{\"offset\":\"423321873\"}">>,

    io:format("~p~n", [Body]),
    %{ok,{{"HTTP/1.1",200,"OK"}, _, Body1}} = 
    %Rez = httpc:request(post, {Url, [],"application/json",Body},[],[]),

    {ok,{{"HTTP/1.1",200,"OK"}, _, Rez}} = httpc:request(post, {Url, [], "application/json",Body}, [], []),
    io:format("request result~p~n", [jsx:decode(unicode:characters_to_binary(Rez))]),

    {noreply, State}.                   


handle_info({get_me, Method}, State) ->  
  
    Url = ?HOST ++ ?TOKEN ++ Method,
    {ok,{{"HTTP/1.1",200,"OK"}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    io:format("request ~p~n", [jsx:decode(unicode:characters_to_binary(Body))]),

    {noreply, State}; 

handle_info(Info, State) ->  
   io:format("Unknown Comand ~n~p~n", [Info]),

    {noreply, State}.          

terminate(_Reason, _State) -> 

    ok.                       

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.              