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
%%% Created : May 15, 2012
%%%-------------------------------------------------------------------------------
-module(error_log_tool_help_web).

-behaviour(cowboy_http_handler).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/3,handle/2]).

%%
%% API Functions
%%

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], help_page(), Req),
    {ok, Req2, State}.

%%
%% Local Functions
%%

help_page() ->
<<"
Error Logger Tool Quick Start Help.

Examples:

    /error_logger                                               - show events for last half of an hour;
    /error_logger?interval=2012-03-27T02:00:00.000Z             - show events since specified Zulu time;
    /error_logger?interval=2012-03-27T02:00:00.000Z&limit=100   - show at most 100 events since specified Zulu time;
    /error_logger?nodes=qsto_node@host.com,qrcv_node@host.com   - show events only from specified nodes;
    /error_logger?nodes_exc=qsto_node@host.com                  - show events from all nodes except specified one;
    /error_logger?severity=error                                - show events of specified severity.

Details:
    The output returned as chunked HTTP reply. To format events the code of standard error formatters is used 
    (form 'kernel' and 'sasl' apps). When specifying time all parts are mandatory and leading zeros are necessary, 
    be careful. Probably later some short cuts for time spec may be add.

    Severity parameter may have the following values:
        warn        - error,error_report,warning_report,warning_msg
        error       - error,error_msg,error_report
        sasl        - supervisor_report,progress,crash_report
        sasl-crash  - crash_report

Author:
    Vyacheslav Vorobyov, <vjache@gmail.com>
">>.

