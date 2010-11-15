%% Author: Eugeniy Meshcheryakov <eugen@debian.org>
%% This file is in the public domain.
-module(txt2gds).
-export([start/0]).

-include("gdsii.hrl").

-record(state, {
  last_token = none :: tuple() | 'none',
  line = 1 :: pos_integer()
}).

-spec start() -> no_return().

start() ->
  run("test.gds"), % FIXME
  init:stop().

-spec run(string()) -> 'ok'.

run(FileName) ->
  case file:open(FileName, [write, binary, raw]) of
    {ok, Dev} ->
      write_gds(Dev),
      file:close(Dev)
  end.

-spec write_gds(file:io_device()) -> 'ok'.

write_gds(Dev) ->
  write_gds(Dev, #state{}).

-spec write_gds(file:io_device(), #state{}) -> 'ok'.

write_gds(Dev, State) ->
  case read_record(Dev, State) of
    {ok, NewState} -> write_gds(Dev, NewState);
    eof -> ok
  end.

get_token(Line) ->
  io:request(standard_io, {get_until, "", gdsdump_lex, token, [Line]}).

-spec read_record(file:io_device(), #state{}) -> {'ok', #state{}} | eof.

read_record(Dev, #state{last_token = {tag, _L, Name}, line = Line}) ->
  read_record_data(Dev, Name, Line);

read_record(Dev, #state{last_token = none, line = Line}) ->
  case get_token(Line) of
    {ok, {tag, _L, Name}, NewLine} ->
      read_record_data(Dev, Name, NewLine);
    {eof, _} -> eof
  end.

-spec read_record_data(file:io_device(), binary(), pos_integer()) -> {'ok', #state{}}.

read_record_data(Dev, Name, Line) ->
  Tag = gdsii:name_to_tag(Name),
  {Data, NewLine, Cont} = case gdsii:type_of_tag(Tag) of
    nodata ->
      {nodata, Line, none};
    D ->
      case get_token(Line) of
        {ok, {colon, _}, NewLine1} ->
          read_data(D, NewLine1)
      end
  end,
  BinData = gdsii:record_to_binary({Tag, Data}),
  file:write(Dev, BinData),
  {ok, #state{last_token = Cont, line = NewLine}}.

read_data(bitarray, Line) ->
  case get_token(Line) of
    {ok, {integer, _L, I}, NewLine} ->
      {{bitarray, I}, NewLine, none}
  end;

read_data(IntType, Line) when IntType =:= int2; IntType =:= int4 ->
  {List, NewLine, Cont} = read_list(integer, Line),
  {{IntType, List}, NewLine, Cont};

read_data(real8, Line) ->
  {List, NewLine, Cont} = read_list(float, Line),
  {{real8, List}, NewLine, Cont};

read_data(ascii, Line) ->
  case get_token(Line) of
    {ok, {string, _L, S}, NewLine} ->
      {{ascii, S}, NewLine, none}
  end.

read_list(Type, Line) ->
  case get_token(Line) of
    {ok, {Type, _, Val}, NewLine} ->
      case get_token(NewLine) of
        {ok, {comma, _}, NewLine1} ->
          {RetList, RetLine, RetCont} = read_list(Type, NewLine1),
          {[Val | RetList], RetLine, RetCont};
        {ok, Cont, NewLine1} ->
          {[Val], NewLine1, Cont};
        {eof, _} -> {[Val], NewLine, none}
      end
  end.
