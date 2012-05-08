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

-define(ECHO(Msg), io:format(standard_error, "~p: ~p~n", [?LINE, Msg])).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    Options = 
        case cowboy_http_req:qs_val(<<"node">>, Req) of
            {undefined,_}   -> [];
            {Val,_}         -> [{node, list_to_atom(binary_to_list(Val))}]
        end ++
            case cowboy_http_req:qs_val(<<"severity">>, Req) of
                {undefined,_}   -> [];
                {Val,_}         -> [list_to_atom(binary_to_list(Val))]
            end,
    case cowboy_http_req:qs_val(<<"interval">>, Req) of
        {undefined,_}            -> Timestamp= logmachine_util:millis_to_now(
                                                 logmachine_util:now_to_millis(now()) - 1800*1000);
        {<<Y:4/binary,_,
           M:2/binary,_,
           D:2/binary,_,
           H:2/binary,_,
           Mn:2/binary,_,
           S:2/binary,".",
           Mls:3/binary,"Z">>,_} -> 
            Timestamp = {{to_int(Y),to_int(M),to_int(D)},
                         {to_int(H),to_int(Mn),to_int(S)}, to_int(Mls)}
    end,
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
%% %%
%% 
%% id() ->
%% 	{Mega, Sec, Micro} = erlang:now(),
%% 	Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
%% 	integer_to_list(Id, 16).