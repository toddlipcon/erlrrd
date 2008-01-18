-module(erlrrd_sup).

-export([start_link/2, start_link/1, start_link/0]).

-behavior(supervisor).

-export([init/1]).

%% @equiv start_link("rrdtool -")
start_link() -> start_link("rrdtool -").

%% @equiv start_link( none, ExtProg )
start_link(ExtProg) -> start_link( erlrrd, ExtProg ).

%% @spec start_link(RegName, ExtProg) ->  Result
%%   RegName = { local, Name } | { global, Name } | Name | none
%%   Name = atom()
%%   ExtProg = string()
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link(RegName, ExtProg) ->
  supervisor:start_link({local,erlrrd_sup}, erlrrd_sup, {RegName, ExtProg}).

init({RegName,ExtProg}) -> 
  io:format("init called w/ ~p ~n", [ {RegName, ExtProg} ]),
  { 
    ok, 
    { 
      {one_for_one, 5, 10 },
      [ 
        { 
          erlrrd,
          { erlrrd, start_link, [ RegName, ExtProg] },
          permanent,
          3000,
          worker,
          [ erlrrd ]
        }
      ]
    }
  }.
