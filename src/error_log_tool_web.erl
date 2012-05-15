%%%-------------------------------------------------------------------
%%% @author <vjache@gmail.com>
%%% @copyright (C) 2011, Vyacheslav Vorobyov.  All Rights Reserved.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc
%%%    TODO: Document it.
%%% @end
%%% Created : Mar 17, 2012
%%%-------------------------------------------------------------------------------
-module(error_log_tool_web).

-behaviour(cowboy_http_handler).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/3, handle/2, terminate/2]).

%%
%% API Functions
%%

-record(state, {interval,nodes_inc,nodes_exc,severity,max_limit,min_limit}).

-define(ECHO(Msg), io:format(standard_error, "~p: ~p~n", [?LINE, Msg])).

init({_Any, http}, Req, []) ->
    State=#state{
                 interval  = parse_interval(qs_val(Req, <<"interval">>, <<>>)),
                 nodes_inc = parse_nodes(   qs_val(Req, <<"nodes">>,    <<>>)),
                 nodes_exc = parse_nodes(   qs_val(Req, <<"nodes_exc">>,<<>>)),
                 severity  = parse_severity(qs_val(Req, <<"severity">>, <<>>)),
                 max_limit = parse_integer( qs_val(Req, <<"limit">>, <<"10000">>)),
                 min_limit = parse_integer( qs_val(Req, <<"limit_min">>, <<"0">>))
                 },
	{ok, Req, State}.

handle(Req, #state{interval=Timestamp,nodes_inc=NodesInc,nodes_exc=NodesExc,max_limit=LimitMax,min_limit=LimitMin,severity=Severity}=State) ->
    Options = [{nodes_exc, NodesExc},
               {nodes_inc, NodesInc},
               {max_limit, LimitMax},
               {min_limit, LimitMin},
               Severity ],
    ZList=logmachine:get_zlist(error_logger, Timestamp),
	Headers = [{'Content-Type', <<"text/plain">>}],
	{ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
    error_log_tool:print_zlist(
      Options,
      fun(Report) -> cowboy_http_req:chunk(Report, Req2) end,
      ZList),
    {ok, Req2, State}.

to_int(Bin) ->
    list_to_integer((binary_to_list(Bin))).

terminate(_Req, _State) ->
	ok.

%%
%% Local Functions
%%

qs_val(Req, Name, Default) ->
    case cowboy_http_req:qs_val(Name, Req) of
        {undefined,_} when is_function(Default,0)   -> Default();
        {undefined,_}   -> Default;
        {Val,_}         -> Val
    end.

parse_interval(<<>>) ->
    logmachine_util:now_add(now(), -1800*1000*1000);
parse_interval(<<Y:4/binary,_,
                 M:2/binary,_,
                 D:2/binary,_,
                 H:2/binary,_,
                 Mn:2/binary,_,
                 S:2/binary,".",
                 Mls:3/binary,"Z">>) ->
    {{to_int(Y),to_int(M),to_int(D)},
     {to_int(H),to_int(Mn),to_int(S)}, to_int(Mls)}.

parse_nodes(NodesBStr) ->
    [ binary_to_existing_atom(B, latin1) || B <- binary:split(NodesBStr, <<",">>, [global,trim])].

parse_severity(<<>>) ->
    all;
parse_severity(BSeverity) ->
    binary_to_existing_atom(BSeverity, latin1).

parse_integer(BInt) ->
    list_to_integer(binary_to_list(BInt)).