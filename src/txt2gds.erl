%% Author: Eugeniy Meshcheryakov <eugen@debian.org>
%% This file is in the public domain.
-module(txt2gds).
-export([start/0, run/1, run/2]).

-include("gdsii.hrl").

-record(state, {
  last_token = none :: tuple() | 'none' | 'eof',
  line = 1 :: pos_integer()
}).

-type io() :: file:io_device() | 'standard_io'.

-spec start() -> no_return().

start() ->
  case init:get_plain_arguments() of
    ["-o", OutFileName | Rest] ->
      case Rest of
        [InFileName] when InFileName /= "-", hd(InFileName) /= $- ->
          run(OutFileName, InFileName);
        ["-"] ->
          run(OutFileName);
        [] ->
          run(OutFileName);
        _ ->
          usage()
      end,
      init:stop();
    _ ->
      usage()
  end.

-spec usage() -> no_return().

usage() ->
  io:put_chars(standard_error, <<"Usage: txt2gds -o file.gds [file.txt]\n">>),
  init:stop(1).

-spec run(string()) -> 'ok'.

run(OutFileName) ->
  case file:open(OutFileName, [write, binary, raw]) of
    {ok, Dev} ->
      write_gds(Dev, standard_io),
      file:close(Dev)
  end.

-spec run(string(), string()) -> 'ok'.

run(OutFileName, InFileName) ->
  case file:open(OutFileName, [write, binary, raw]) of
    {ok, Dev} ->
      case file:open(InFileName, [read, read_ahead]) of
        {ok, Input} ->
          write_gds(Dev, Input),
          file:close(Input),
          file:close(Dev)
      end
  end.

-spec write_gds(file:io_device(), io()) -> 'ok'.

write_gds(Dev, Input) ->
  write_gds(Dev, Input, #state{}).

-spec write_gds(file:io_device(), io(), #state{}) -> 'ok'.

write_gds(_Dev, _Input, #state{last_token = eof}) ->
  ok;

write_gds(Dev, Input, State) ->
  case read_record(Dev, Input, State) of
    {ok, NewState} -> write_gds(Dev, Input, NewState);
    eof -> ok
  end.

-spec get_token(io(), pos_integer()) -> any().

get_token(Input, Line) ->
  io:request(Input, {get_until, "", gdsdump_lex, token, [Line]}).

-spec read_record(file:io_device(), io(), #state{}) -> {'ok', #state{}} | eof.

read_record(Dev, Input, #state{last_token = {tag, _L, Name}, line = Line}) ->
  read_record_data(Dev, Input, Name, Line);

read_record(Dev, Input, #state{last_token = none, line = Line}) ->
  case get_token(Input, Line) of
    {ok, {tag, _L, Name}, NewLine} ->
      read_record_data(Dev, Input, Name, NewLine);
    {eof, _} -> eof
  end.

-spec read_record_data(file:io_device(), io(), binary(), pos_integer()) -> {'ok', #state{}}.

read_record_data(Dev, Input, Name, Line) ->
  Tag = gdsii:name_to_tag(Name),
  {Data, NewLine, Cont} = case gdsii:type_of_tag(Tag) of
    nodata ->
      {nodata, Line, none};
    D ->
      case get_token(Input, Line) of
        {ok, {colon, _}, NewLine1} ->
          read_data(Input, D, NewLine1)
      end
  end,
  BinData = gdsii:record_to_binary({Tag, Data}),
  file:write(Dev, BinData),
  {ok, #state{last_token = Cont, line = NewLine}}.

read_data(Input, bitarray, Line) ->
  case get_token(Input, Line) of
    {ok, {integer, _L, I}, NewLine} ->
      {{bitarray, I}, NewLine, none}
  end;

read_data(Input, IntType, Line) when IntType =:= int2; IntType =:= int4 ->
  {List, NewLine, Cont} = read_list(Input, integer, Line),
  {{IntType, List}, NewLine, Cont};

read_data(Input, real8, Line) ->
  {List, NewLine, Cont} = read_list(Input, float, Line),
  {{real8, List}, NewLine, Cont};

read_data(Input, ascii, Line) ->
  case get_token(Input, Line) of
    {ok, {string, _L, S}, NewLine} ->
      {{ascii, S}, NewLine, none}
  end.

-spec read_list(io(), atom(), pos_integer()) -> {list(), pos_integer(), tuple() | 'eof'}.

read_list(Input, Type, Line) ->
  case get_token(Input, Line) of
    {ok, {Type, _, Val}, NewLine} ->
      case get_token(Input, NewLine) of
        {ok, {comma, _}, NewLine1} ->
          {RetList, RetLine, RetCont} = read_list(Input, Type, NewLine1),
          {[Val | RetList], RetLine, RetCont};
        {ok, Cont, NewLine1} ->
          {[Val], NewLine1, Cont};
        {eof, _} -> {[Val], NewLine, eof}
      end
  end.
