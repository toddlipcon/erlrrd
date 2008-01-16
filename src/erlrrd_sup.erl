-module(erlrrd_sup).

-export([start_link/1]).

-behavior(supervisor).

-export([init/1]).

start_link(ExtProg) ->
  supervisor:start_link(erlrrd_sup, ExtProg).

init(ExtProg) -> 
  { 
    ok, 
    { 
      {simple_one_for_one, 5, 10 },
      [ 
        { 
          erlrrd,
          { erlrrd, start_link, [ ExtProg] },
          permanent,
          3000,
          worker,
          [ erlrrd ]
        }
      ]
    }
  }.
