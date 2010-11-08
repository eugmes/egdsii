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

-spec print_tag(integer(), gdsii:record_data()) -> 'ok'.

print_tag(Tag, Data) ->
  format_data(gdsii:name_of_tag(Tag), Data).

-spec format_data(binary(), gdsii:data_type()) -> 'ok'.

format_data(Name, nodata) ->
  io:put_chars([Name, <<"\n">>]);

format_data(Name, {ascii, String}) ->
  io:put_chars([Name, <<": \"">>, String, <<"\"\n">>]); % TODO escape special chars in String

format_data(Name, {bitarray, Bits}) ->
  io:fwrite("~s: 0x~4.16.0B~n", [Name, Bits]);

format_data(Name, {int2, List}) ->
  io:put_chars([Name, <<": ">>, format_num_list(fun integer_to_list/1, List), <<"\n">>]);

format_data(Name, {int4, List}) ->
  io:put_chars([Name, <<": ">>, format_num_list(fun integer_to_list/1, List), <<"\n">>]);

format_data(Name, {real8, List}) ->
  io:put_chars([Name, <<": ">>, format_num_list(fun(N) -> io_lib:format("~g", [N]) end, List), <<"\n">>]).

format_num_list(F, L) ->
  [_X|Xs] = lists:foldl(fun(E, Acc) -> [<<", ">>, F(E) | Acc] end, [], L),
  iolist_to_binary(lists:reverse(Xs)).

-spec show_gds(file:io_device()) -> 'ok'.

show_gds(Dev) ->
  case gdsii:read_record(Dev) of
    {Tag, Data} when Tag == ?ENDLIB ->
      print_tag(Tag, Data);
    {Tag, Data} ->
      print_tag(Tag, Data),
      show_gds(Dev)
  end.
