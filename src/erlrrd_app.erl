-module(erlrrd_app).

-export([start/0, stop/0]).
-behavior(application).
-export([start/2, stop/1]).


start() -> 
  application:start(erlrrd).

start(_Type, _Args) -> 
  case application:get_env(erlrrd, rrdtoolcmd) of
    { ok, RRDToolCmd } -> 
      erlrrd_sup:start_link(RRDToolCmd);
    undefined -> 
      erlrrd_sup:start_link()
  end.

stop() -> 
  application:stop(erlrrd).

stop(_State) -> 
  ok. 
