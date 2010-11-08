%% Author: Eugeniy Meshcheryakov <eugen@debian.org>
%% This file is in the public domain.
-module(gds2txt).
-export([main/0, main/1, run/1]).

-include("gdsii.hrl").

-spec main() -> no_return().

main() ->
  io:put_chars(standard_error, <<"Usage: gds2txt file.gds\n">>),
  init:stop(1).

-spec main(list()) -> no_return().

main([FileName]) ->
  run(FileName),
  init:stop();

main(_Args) ->
  main().

run(FileName) ->
  case file:open(FileName, [read, raw, binary, read_ahead]) of
    {ok, Dev} ->
      show_gds(Dev),
      file:close(Dev)
  end.

-spec format_tag(integer(), gdsii:record_data(), iolist()) -> iolist().

format_tag(Tag, Data, Acc) ->
  [Acc, format_data(gdsii:name_of_tag(Tag), Data)].

-spec format_data(binary(), gdsii:data_type()) -> iolist().

format_data(Name, nodata) ->
  [Name, <<"\n">>];

format_data(Name, {ascii, String}) ->
  [Name, <<": \"">>, String, <<"\"\n">>]; % TODO escape special chars in String

format_data(Name, {bitarray, Bits}) ->
  [Name, <<": 0x">>, integer_to_list(Bits, 16), <<"\n">>];

format_data(Name, {int2, List}) ->
  [Name, <<": ">>, format_num_list(fun integer_to_list/1, List), <<"\n">>];

format_data(Name, {int4, List}) ->
  [Name, <<": ">>, format_num_list(fun integer_to_list/1, List), <<"\n">>];

format_data(Name, {real8, List}) ->
  [Name, <<": ">>, format_num_list(fun(N) -> io_lib:format("~g", [N]) end, List), <<"\n">>].

format_num_list(F, L) ->
  [_X|Xs] = lists:foldl(fun(E, Acc) -> [<<", ">>, F(E) | Acc] end, [], L),
  lists:reverse(Xs).

-define(FLUSH_CNT, 40).

-spec show_gds(file:io_device()) -> 'ok'.

show_gds(Dev) ->
  show_gds(Dev, [], ?FLUSH_CNT).

show_gds(Dev, Acc, N) ->
  case gdsii:read_record(Dev) of
    {Tag, Data} when Tag == ?ENDLIB ->
      Acc1 = format_tag(Tag, Data, Acc),
      io:put_chars(iolist_to_binary(Acc1));
    {Tag, Data} ->
      Acc1 = format_tag(Tag, Data, Acc),
      case N of
        0 ->
          io:put_chars(iolist_to_binary(Acc1)),
          show_gds(Dev, [], ?FLUSH_CNT);
        _ ->
          show_gds(Dev, Acc1, N - 1)
      end
  end.
