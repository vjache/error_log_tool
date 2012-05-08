%% Author: vvorobyov
%% Created: Mar 16, 2012
%% Description: TODO: Add description to disk_log_tool
-module(error_log_tool).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([cat/1, cat/2, visit_log/2, filter/2, print_log/3, print_zlist/3]).

-define(REPORT_SEPARATOR,"~n### ~p -----------------------------------------------------------------------~n").

%%
%% API Functions
%%

cat(LogFilename) ->
    cat([], LogFilename).

cat(Options, LogFilename) ->
    print_log(
        Options, 
        fun(IoList) ->
            io:format(standard_io, "~s",[IoList])
        end, 
        LogFilename).

print_log(Options, PrintFun, LogFilename) when is_function(PrintFun, 1) ->
    visit_log(
      fun(LogZList)->
        print_zlist(Options, PrintFun, LogZList)
      end, LogFilename).

print_zlist(Options, PrintFun, LogZList) when is_function(PrintFun, 1) ->
    LogZList1=filter(Options, 
            zlists:ziph(zlists:seq(1, 1000000000000, 1), 
            LogZList)),
        LogZList2=zlists:map(
            fun({_N,{NTime,Evt}})->
                {_N,{calendar:now_to_universal_time(NTime),Evt}}
            end, LogZList1),
              zlists:foreach(
                fun(E) -> print_evt(PrintFun,E) end,
                LogZList2 ).

print_evt(PrintFun, {N, TaggedEvt}) ->
    PrintFun(io_lib:format(?REPORT_SEPARATOR, [N])),
    Report=error_log_tool_fmt:format_event(TaggedEvt),
    PrintFun(Report).

visit_log(Fun, LogFilename) when is_function(Fun, 1) ->
    {ok,Log}=disk_log:open(
               [{name,LogFilename},
                {file,LogFilename},
                {mode,read_only}]),
    try Z=zlists_disk_log:read(Log),
        Fun(Z)
    after
        disk_log:close(Log)
    end.

filter([]=_Options,LogZList) ->
    LogZList;
filter([Opt|Tail]=_Options,LogZList) ->
    case Opt of
        'sasl' ->
            Z=zlists:filter(
                fun({_N,{_, {_, _, {_, Type, _}} }})->
                        lists:member(Type, [supervisor_report,progress,crash_report]);
                   (_) -> false
                end, LogZList);
        'sasl-crash' ->
            Z=zlists:filter(
                fun({_N,{_, {_, _, {_, Type, _}} }})->
                        lists:member(Type, [crash_report]);
                   (_) -> false
                end, LogZList);
        'error' ->
            Z=zlists:filter(
                fun({_N,{_, {Type, _, {_, _, _}} }})->
                        lists:member(Type, [error,error_msg,error_report]);
                   (_) -> false
                end, LogZList);
        'warn' ->
            Z=zlists:filter(
                fun({_N,{_, {Type, _, {_, _, _}} }})->
                        lists:member(Type, [error,error_report,warning_report,warning_msg]);
                   (_) -> false
                end, LogZList);
        {node, Node} ->
            Z=zlists:filter(
                fun({_N,{_, {_, Node1, {_, _, _}} }})->
                        Node1 == Node;
                   (_) -> false
                end, LogZList)
    end,
    filter(Tail,Z).

%%
%% Local Functions
%%


