%%%-------------------------------------------------------------------
%%% File    : dtop.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created :  5 Sep 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(dtop).

-export([start/1,stop/0,sort/1]).

start(Node) when atom(Node) -> start([Node]);
start([Node]) -> prf:start(dtop,Node,dtopConsumer).

stop() -> prf:stop(dtop).

sort(Sort) -> dtop ! {config,{sort,Sort}}.


%% f().

%% Ftc=fun(Tag,OD)->orddict:fetch(Tag,OD) end.
%% PI = fun() -> [orddict:from_list(erlang:port_info(P))||P<-erlang:ports()] end.

%% DT = fun()-> orddict:from_list([{{hd(string:tokens(filename:basename(Ftc(name,PIs))," ")),Ftc(id,PIs)},{Ftc(input,PIs),Ftc(output,PIs)}}||PIs<-PI()]) end.

%% FLT = fun(G,[_X|T1],[_X|T2]) -> G(G,T1,T2);(G,[{A,{I1,O1}}|T1],[{A,{I2,O2}}|T2]) -> [{A,I1-I2,O1-O2}|G(G,T1,T2)]; (G,_,_)-> [] end.
          

%% IF = fun(G,OD) -> ND=DT(), receive stop->ok after 2000 -> io:fwrite("~p~n",[FLT(FLT,ND,OD)]),G(G,ND) end end.

%% SIF = fun()-> IF(IF,[]) end.
%% P = spawn(SIF).

%% P!stop.
