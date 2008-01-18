-module(erlrrd_sup).

-export([start_link/1, start_link/0]).

-behavior(supervisor).

-export([init/1]).


%% @spec start_link(RRDToolCmd) ->  Result
%%   RRDToolCmd = string()
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link(RRDToolCmd) ->
  supervisor:start_link(erlrrd_sup, [RRDToolCmd]).

%% @spec start_link() ->  Result
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link() -> 
  supervisor:start_link(erlrrd_sup, []).

init(Args) -> 
  { 
    ok, 
    { 
      {one_for_one, 5, 10 },
      [ 
        { 
          erlrrd,
          { erlrrd, start_link, Args },
          permanent,
          3000,
          worker,
          [ erlrrd ]
        }
      ]
    }
  }.
