-module(erlrrd_sup).

-export([start_link/1]).

-behavior(supervisor).

-export([init/1]).

%% @equiv start_link("rrdtool -")
start_link() -> start_link("rrdtool -").

%% @equiv start_link( none, ExtProg )
start_link(ExtProg) -> start_link( none, ExtProg ).

%% @spec start_link(RegName, ExtProg) ->  Result
%%   RegName = { local, Name } | { global, Name } | Name | none
%%   Name = atom()
%%   ExtProg = string()
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link(RegName, ExtProg) ->
  supervisor:start_link(erlrrd_sup, {RegName, ExtProg}).

init({RegName,ExtProg}) -> 
  { 
    ok, 
    { 
      {simple_one_for_one, 5, 10 },
      [ 
        { 
          erlrrd,
          { erlrrd, start_link, [ {RegName, ExtProg}] },
          permanent,
          3000,
          worker,
          [ erlrrd ]
        }
      ]
    }
  }.
