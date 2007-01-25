%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : redbg.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 24 Jan 2007 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(redbug).

-export([help/0]).
-export([start/3,start/4,start/5]).
-export([stop/0]).

-import(lists,[foldl/3,usort/1,reverse/1,foreach/2]).

-define(LOG(T), prf:log(process_info(self()),T)).

help() ->
  strs(["redbug:start(Time,Msgs,Trc[,Proc[,Targ]])",
        "Time: integer() [ms]",
        "Msgs: integer() [#]",
        "Trc: list('send'|'receive'|{M,F}|{M,F,RestrictedMatchSpec})",
        "Proc: 'all'|pid()|atom(Regname)|{'pid',I2,I3}",
        "Targ: node()"]).

start(Time,Msgs,Trc) -> start(Time,Msgs,Trc,all).

start(Time,Msgs,Trc,Proc) -> start(Time,Msgs,Trc,Proc,node()).

start(Time,Msgs,Trc,Proc,Targ)  -> 
  try 
    register(rdbg, spawn(fun init/0)),
    rdbg ! {start,{Time,Msgs,Trc,Proc,Targ}}
  catch C:R -> {C,R}
  end.

stop() ->
  rdbg ! {stop,[]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  process_flag(trap_exit,true),
  receive 
    {start,{Time,Msgs,Trc,Proc,Targ}} -> 
      Conf = pack(Time,Msgs,Trc,Proc),
      prf:start(hygglo,Targ,redbugConsumer),
      prf:config(hygglo,collectors,{start,{self(),Conf}}),
      iloop()
  end.

iloop() ->
  receive
    {prfTrc,{starting,TrcPid}} ->
      loop(TrcPid);
    {prfTrc,{already_started,_TrcPid}} ->
      ?LOG(already_started);
    {stop,Args} ->
      prf:config(hygglo,collectors,{stop,{self(),Args}});
    {'EXIT',Pid,R} -> 
      ?LOG([{exited,Pid},{reason,R}]);
    {'EXIT',R} -> 
      ?LOG([exited,{reason,R}]);
    X ->
      ?LOG([{unknown_message,X}])
  end.

loop(TrcPid) ->
  receive
    {prfTrc,{stopping,TrcPid}} ->
      ok;
    {prfTrc,{not_started,TrcPid}} ->
      ?LOG(not_started);
    {stop,Args} ->
      prf:config(hygglo,collectors,{stop,{self(),Args}});
    {'EXIT',TrcPid,R} -> 
      ?LOG([tracer_died,{reason,R}]);
    {'EXIT',R} -> 
      ?LOG([exited,{reason,R}]);
    X ->
      ?LOG([{unknown_message,X}])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conf = {time,flags,rtps,procs,where}
%%% Where = {term_buffer,Pid,Count} | {term_stream,Pid,Count} |
%%%         {file,File,Size} | {ip,Port,Queue}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack(Time,Msgs,Trc,Proc) ->
  {Flags,RTPs} = foldl(fun chk_trc/2,{[],[]},ass_list(Trc)),
  dict:from_list([{time,chk_time(Time)},
                  {flags,[call,timestamp|Flags]},
                  {rtps,RTPs},
                  {procs,chk_proc(Proc)},
                  {where,{term_stream,ass_printer(),chk_msgs(Msgs)}}]).

chk_time(Time) when is_integer(Time) -> Time;
chk_time(X) -> exit({bad_time,X}).

chk_msgs(Msgs) when is_integer(Msgs) -> Msgs;
chk_msgs(X) -> exit({bad_msgs,X}).

chk_trc('send',{Flags,RTPs}) -> {['send'|Flags],RTPs};
chk_trc('receive',{Flags,RTPs}) -> {['receive'|Flags],RTPs};
chk_trc(RTP,{Flags,RTPs}) when is_tuple(RTP) -> {Flags,[chk_rtp(RTP)|RTPs]};
chk_trc(X,_) -> exit({bad_trc,X}).

chk_proc(Pid) when is_pid(Pid) -> Pid;
chk_proc(Atom) when is_atom(Atom)-> Atom;
chk_proc({pid,I1,I2}) when is_integer(I1), is_integer(I2) -> {pid,I1,I2};
chk_proc(X) -> exit({bad_proc,X}).

chk_rtp({M,F}) when atom(M), atom(F), M/='_' -> {{M,F,'_'},[],[local]};
chk_rtp({M,F,MS}) when atom(M), atom(F), M/='_' -> {{M,F,'_'},ms(MS),[local]};
chk_rtp(X) -> exit({bad_rtp,X}).

ms(MS) -> foldl(fun msf/2, [{'_',[],[]}], MS).

msf(stack, [{Head,Cond,Body}]) -> [{Head,Cond,[{message,{process_dump}}|Body]}];
msf(return, [{Head,Cond,Body}]) -> [{Head,Cond,[{return_trace}|Body]}];
msf(Head, [{_,Cond,Body}]) when tuple(Head)-> [{Head,Cond,Body}];
msf(X,_) -> exit({bad_match_spec,X}).

ass_list(L) when is_list(L) -> usort(L);
ass_list(X) -> [X].

ass_printer() ->
  Self = self(),
  spawn_link(fun()->printi(Self) end).

strs([]) -> ok;
strs([H|T]) -> io:fwrite("~s~n",[H]),strs(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printi(Pid) ->
  erlang:monitor(process,Pid),
  printl().
printl() ->
  receive
    {'DOWN',_Ref,process,_Pid,_Reason} -> ok;
    X -> outer(X), printl()
  end.

outer([]) -> ok;
outer([Msg|Msgs]) ->
  case Msg of
    {call,{MFA,Bin},PI,TS} when is_binary(Bin) ->
      io:fwrite("~p~n",[{call,MFA,PI,TS}]),
      foreach(fun(L)->io:fwrite("  ~p~n",[L]) end, stak(Bin));
    MSG -> 
      io:fwrite("~p~n", [MSG])
  end,
  outer(Msgs).

%%% call stack handler
stak(Bin) ->
  foldl(fun munge/2,[],string:tokens(binary_to_list(Bin),"\n")).

munge(I,Out) ->
  case reverse(I) of
    "..."++_ -> [truncated|Out];
    _ -> 
      case string:str(I, "Return addr") of
        0 -> 
          case string:str(I, "cp = ") of
            0 -> Out;
            _ -> [mfaf(I)|Out]
          end;
        _ ->
          case string:str(I, "erminate process normal") of
            0 -> [mfaf(I)|Out];
            _ -> Out
          end
      end
  end.

mfaf(I) ->
  [_, C|_] = string:tokens(I,"()+"),
  case string:tokens(C,":/ ") of
    [M,F,A] ->
      case catch {list_to_atom(M),list_to_atom(F),list_to_integer(A)} of
        {'EXIT',_} -> C;
        X -> X
      end;
    ["unknown","function"] ->
      unknown_function
  end.
