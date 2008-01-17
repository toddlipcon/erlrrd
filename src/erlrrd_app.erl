-module(erlrrd_app).

-behavior(application).
-export([start/2, stop/1]).


start(_Type, _Args) -> 
  case application:get_env(erlrrd, extprog) of
    { ok, ExtProg } -> 
      erlrrd_sup:start_link({erlrrd, ExtProg});
    true -> 
      erlrrd_sup:start_link({erlrrd, "rrdtool -"})
  end.

stop(_State) -> 
  ok. 
