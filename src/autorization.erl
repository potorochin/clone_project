-module(autorization).
-export([start/0]).

start() -> 
	application:start(autorization_app).