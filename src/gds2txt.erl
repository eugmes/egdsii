-module(gds2txt).
-export([main/0, main/1]).

-include("gdsii.hrl").

-spec main() -> no_return().

main() ->
  io:put_chars(standard_error, "Usage: gds2txt file.gds\n"),
  halt(1).

-spec main(list()) -> no_return().

main([FileName]) ->
  case file:open(FileName, [read, raw, binary, read_ahead]) of
    {ok, Dev} ->
      show_gds(Dev),
      file:close(Dev)
  end,
  halt();

main(_Args) ->
  main().

-spec print_tag(integer(), gdsii:record_data()) -> 'ok'.

print_tag(Tag, Data) ->
  format_data(gdsii:name_of_tag(Tag), Data).

-spec format_data(binary(), gdsii:data_type()) -> 'ok'.

format_data(Name, nodata) ->
  io:fwrite("~s~n", [Name]);

format_data(Name, {ascii, String}) ->
  io:fwrite("~s: \"~s\"~n", [Name, String]);

format_data(Name, {bitarray, Bits}) ->
  io:fwrite("~s: 0x~4.16.0B~n", [Name, Bits]);

format_data(Name, {int2, List}) ->
  io:fwrite("~s: ~s~n", [Name, format_num_list(fun integer_to_list/1, List)]);

format_data(Name, {int4, List}) ->
  io:fwrite("~s: ~s~n", [Name, format_num_list(fun integer_to_list/1, List)]);

format_data(Name, {real8, List}) ->
  io:fwrite("~s: ~s~n", [Name, format_num_list(fun(N) -> io_lib:format("~g", [N]) end, List)]).

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
