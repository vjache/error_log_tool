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
-module(error_log_tool_fmt).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([pprint/1, format_event/1]).

%%
%% API Functions
%%

pprint({NowTime, Evt}) ->
    io:format(standard_io, "~s", 
              [format_event({calendar:now_to_universal_time(NowTime),Evt})]).

%%
%% Local Functions
%%
format_event({Time, {error, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(add_node(Format,Pid), Args) of
    S when is_list(S) ->
        format(T ++ S);
    _ ->
        F = add_node("ERROR: ~p - ~p~n", Pid),
        format(T ++ F, [Format,Args])
    end;
format_event({Time, {emulator, _GL, Chars}}) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(Chars, []) of
    S when is_list(S) ->
        format(T ++ S);
    _ ->
        format(T ++ "ERROR: ~p ~n", [Chars])
    end;
format_event({Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(maybe_utc(Time)),
    format(T ++ add_node("~p~n",Pid),[Info]);
format_event({Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(maybe_utc(Time)),
    S = format_report(Rep),
    format(T ++ S ++ add_node("", Pid));
format_event({Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    S = format_report(Rep),
    format(T ++ S ++ add_node("", Pid));
format_event({Time, {info_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
    S when is_list(S) ->
        format(T ++ S);
    _ ->
        F = add_node("ERROR: ~p - ~p~n", Pid),
        format(T ++ F, [Format,Args])
    end;
format_event({Time, {warning_report, _GL, {Pid, std_warning, Rep}}}) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    S = format_report(Rep),
    format(T ++ S ++ add_node("", Pid));
format_event({Time, {warning_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
    S when is_list(S) ->
        format(T ++ S);
    _ ->
        F = add_node("ERROR: ~p - ~p~n", Pid),
        format(T ++ F, [Format,Args])
    end;
format_event({_Time, _Error}=Evt) ->
    sasl_report:format_report(
                standard_io, all, 
                Evt).

maybe_utc(Time) ->
    UTC = case application:get_env(sasl, utc_log) of
              {ok, Val} ->
                  Val;
              undefined ->
                  %% Backwards compatible:
                  case application:get_env(stdlib, utc_log) of
                      {ok, Val} ->
                          Val;
                      undefined ->
                          false
                  end
          end,
    if
        UTC =:= true ->
            {utc, calendar:local_time_to_universal_time(Time)};
        true -> 
            Time
    end.

format(String)       -> io_lib:format(String, []).
format(String, Args) -> io_lib:format(String, Args).

format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
    true ->
        io_lib:format("~s~n",[Rep]);
    _ ->
        format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].

add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) =/= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
    true -> string_p1(T);
    _    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) -> write_time(Time, "ERROR REPORT").
write_time({utc,{{Y,Mo,D},{H,Mi,S}}},Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC ===~n",
          [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]);
write_time({{Y,Mo,D},{H,Mi,S}},Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s ===~n",
          [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]).

t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".
t1([X]) -> [$0,X];
t1(X)   -> X.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".