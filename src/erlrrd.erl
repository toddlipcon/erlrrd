-module(erlrrd).

-export([create/1, update/1, updatev/1, dump/1, restore/1, last/1,
         first/1, info/1, fetch/1, tune/1, resize/1, xport/1,
         graph/1, lastupdate/1, ls/0, cd/1, mkdir/1, pwd/0, quit/0 
         ]).

-export([start_link/0, start/0]).
-export([stop/0]).
-export([combine/1, c/1]).

-export([join/2]).
%-export([do/2]).
-export([has_newline/1]).
-export([iodata_to_deeplist/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record( state, { port }  ).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start()      -> gen_server:start     ({local, ?MODULE}, ?MODULE, [], []).
stop()       -> gen_server:call      (?MODULE, stop).


c(Args) -> combine(Args).
combine(Args) -> 
    join([ [ "\"", X, "\"" ] || X <- Args ], " ").

join([Head | [] ], _Sep) ->
  [Head];
join([Head | Tail], Sep) ->
  [ Head, Sep | join(Tail, Sep) ].



% has_newline([]) -> false;
% has_newline(B) when is_binary (B) -> 
%   has_newline_binary(B);
% has_newline([ H |  T]) 
%   when is_list(H) ->
%     case has_newline(H) of
%       true -> true;
%       false -> has_newline(T)
%     end;
% has_newline([ H | _T]) 
%   when is_integer(H), H =:= $\n -> true;
% has_newline([ H | _T]) 
%   when is_binary(H) -> 
%     has_newline_binary(H);
% has_newline([_H |  T]) ->
%   has_newline(T).
% 
% has_newline_binary(<<>>) -> false;
% has_newline_binary(<<H:8,_T/binary>> = B)
%   when is_binary(B), H =:= $\n -> true;
% has_newline_binary(<<_H:8,T/binary>>) ->
%   has_newline(T).

has_newline([]) -> false;
has_newline(<<>>) -> false;
has_newline([ H |  T]) 
  when is_list(H); is_binary(H) ->
    case has_newline(H) of
      true -> true;
      false -> has_newline(T)
    end;
has_newline([ H | T]) when is_integer(H) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end;
has_newline(<<H:8,T/binary>>) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end.

do(Command, Args) -> 
  case has_newline(Args) of
    true  -> { error, "No newlines" };
    false -> gen_server:call (?MODULE, { do, Command, Args } ) 
  end.
% rrd commands
create     (Args) when is_list(Args) -> do(create,     Args).
update     (Args) when is_list(Args) -> do(update,     Args).
updatev    (Args) when is_list(Args) -> do(updatev,    Args).
dump       (Args) when is_list(Args) -> do(dump,       Args).
restore    (Args) when is_list(Args) -> do(restore,    Args).
last       (Args) when is_list(Args) -> do(last,       Args).
lastupdate (Args) when is_list(Args) -> do(lastupdate, Args).
first      (Args) when is_list(Args) -> do(first,      Args).
info       (Args) when is_list(Args) -> do(info,       Args).
fetch      (Args) when is_list(Args) -> do(fetch,      Args).
tune       (Args) when is_list(Args) -> do(tune,       Args).
resize     (Args) when is_list(Args) -> do(resize,     Args).
xport      (Args) when is_list(Args) -> do(xport,      Args).
graph      (Args) when is_list(Args) -> 
  % TODO: regexp:match needs a string, will break w/binarys
  %   lists:flatten needs a deeplist, will break with iolists
  %   fix both of these.
  case regexp:match(lists:flatten([ [ " ", X ] || X <- Args ]), " -( |$)") of
    { match, _, _ } -> 
      % graph to stdout will break this Ports parsing of reponses..
      { error, "Graphing to stdout not supported." };
    nomatch -> 
      do(graph, Args)
  end.

% rrd "remote" commands
cd         (Arg)  when is_list(Arg) -> do(cd,         Arg).
mkdir      (Arg)  when is_list(Arg) -> do(mkdir,      Arg).
ls         ()     -> do(ls,         []  ).
pwd        ()     -> do(pwd,        []  ).
quit() -> stop().

iodata_to_deeplist([ H | B ]) 
  when is_binary(B) -> [ iodata_to_deeplist(H) | [B] ];
iodata_to_deeplist([H | T] ) ->
  [ iodata_to_deeplist(H) | iodata_to_deeplist(T) ];
iodata_to_deeplist(X) -> X.

%iodata_has(_L, []) -> true;
%iodata_has([H|T], For) 
%  when is_list(H); is_binary(H) -> 
%    case iodata_has(H, For) of
%      true -> true;
%      false -> iodata_has(T, For)
%    end;
%iodata_has([H|T], [Hf,Tf]) 
%  when is_integer(H),  ->
    

init(_Args) -> 
  % TODO choose mechanism to allow user to set rrdtool
  ExtProg = "rrdtool -",
  process_flag(trap_exit, true),
  Port = erlang:open_port({spawn, ExtProg}, [ {line, 10000}, eof, exit_status, stream ] ),
  {ok, #state{port = Port}}.

%%
%% handle_call
%%
handle_call({do, Action, Args }, _From, #state{port = Port} = State) ->
    Line = [ erlang:atom_to_list(Action), " ", Args , "\n"],
    io:format("command to send: ~p\n", [ Line ] ),
    port_command(Port, Line),
    case collect_response(Port) of
        {response, Response} -> 
            {reply, { ok, Response }, State};
        { error, timeout } ->
            {stop, port_timeout, State};
        { error, Error } -> 
            {reply, { error, Error  }, State}
    end;

handle_call(stop, _From, State) -> 
  {stop, normal, stopped, State}.

%% handle_cast
handle_cast(_Msg, State) -> {noreply, State}.

%% handle_info
handle_info(_Info, State) -> {noreply, State}.

%% terminate
terminate(_Reason, _State) -> ok.

%% code_change
code_change(OldVsn, State, _Extra) -> 
  io:format("Yo! code load time!!! ~p ~n",[OldVsn]), 
  {ok, State}.

%%%%

%
collect_response(Port) ->
    collect_response(Port, [], []).

collect_response( Port, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "OK u:" ++ _T }}} ->
            {response, lists:reverse(RespAcc)};
        {Port, {data, {eol, "ERROR: " ++ Error }}} ->
            {error, [ Error, lists:reverse(RespAcc)]};
        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], []);
        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc])

    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after 3000 ->  % TODO user configurable timeout.
            { error, timeout }
    end.
