-module(gdsii).
-export([read_record/1, name_of_tag/1]).

-include("gdsii.hrl").

-type data_type() :: 'nodata' | 'bitarray' | 'int2' | 'int4' | 'real8' | 'ascii'.
-type record_data() ::
    'nodata'
  | {'bitarray',  non_neg_integer() }
  | {'int2', [integer()] }
  | {'int4', [integer()] }
  | {'real8', [float()] }
  | {'ascii', binary()}.

-export_type([data_type/0, record_data/0]).

-spec read_record(file:io_device()) -> {non_neg_integer(), record_data()}.

read_record(Dev) ->
  case file:read(Dev, 4) of
    {ok, Data} when byte_size(Data) == 4 ->
      case Data of
        <<DataSize:16, Tag:16>> when DataSize >= 4 ->
          {Tag, read_data(Dev, DataSize - 4, Tag)}
      end
    % maybe handle errors
  end.

-spec type_of_tag(non_neg_integer()) -> data_type().

type_of_tag(Tag) ->
  case (Tag band 16#F) of
    ?NODATA -> nodata;
    ?BITARRAY -> bitarray;
    ?INT2 -> int2;
    ?INT4 -> int4;
    ?REAL4 -> real4;
    ?REAL8 -> real8;
    ?ASCII -> ascii
  end.

% TODO optimize
int2_from_binary(<<>>) -> [];

int2_from_binary(<<Int2:16/signed, Rest/binary>>) ->
  [Int2] ++ int2_from_binary(Rest).

% TODO optimize
int4_from_binary(<<>>) -> [];

int4_from_binary(<<Int4:32/signed, Rest/binary>>) ->
  [Int4] ++ int4_from_binary(Rest).

% TODO optimize
real8_from_binary(<<>>) -> [];

real8_from_binary(<<Sgn:1, Exp:7, Mant:56, Rest/binary>>) ->
  FSign = case Sgn of
    0 -> 1.0;
    1 -> -1.0
  end,
  % TODO use something like ldexp()
  [FSign * Mant * math:pow(2, (4 * (Exp - 64) - 56))] ++ real8_from_binary(Rest).

-spec read_data(file:io_device(), non_neg_integer(), non_neg_integer()) -> record_data().

read_data(Dev, Size, Tag) ->
  read_data_type(Dev, Size, type_of_tag(Tag)).

-spec read_data_type(file:io_device(), non_neg_integer(), data_type()) -> record_data().

read_data_type(_Dev, Size, nodata) ->
  case Size of
    0 -> nodata
  end;

read_data_type(Dev, Size, Type) ->
  case file:read(Dev, Size) of
    {ok, Data} when byte_size(Data) == Size ->
      parse_data(Size, Data, Type)
  end.

parse_data(2, Data, bitarray) ->
  case Data of
    <<Bitarray:16>> -> {bitarray, Bitarray}
  end;

parse_data(Size, Data, int2) when Size > 0, Size rem 2 == 0 ->
  {int2, int2_from_binary(Data)};

parse_data(Size, Data, int4) when Size > 0, Size rem 4 == 0 ->
  {int4, int4_from_binary(Data)};

parse_data(Size, Data, real8) when Size > 0, Size rem 8 == 0 ->
  {real8, real8_from_binary(Data)}; % TODO

parse_data(Size, Data, ascii) when Size > 0, Size rem 2 == 0 ->
  Trimmed = case binary:last(Data) of
    0 -> binary:part(Data, 0, Size - 1);
    _ -> Data
  end,
  {ascii, Trimmed}.

-spec name_of_tag(non_neg_integer()) -> binary().

name_of_tag(Tag) ->
  case Tag of
    ?HEADER -> <<"HEADER">>;
    ?BGNLIB -> <<"BGNLIB">>;
    ?LIBNAME -> <<"LIBNAME">>;
    ?UNITS -> <<"UNITS">>;
    ?ENDLIB -> <<"ENDLIB">>;
    ?BGNSTR -> <<"BGNSTR">>;
    ?STRNAME -> <<"STRNAME">>;
    ?ENDSTR -> <<"ENDSTR">>;
    ?BOUNDARY -> <<"BOUNDARY">>;
    ?PATH -> <<"PATH">>;
    ?SREF -> <<"SREF">>;
    ?AREF -> <<"AREF">>;
    ?TEXT -> <<"TEXT">>;
    ?LAYER -> <<"LAYER">>;
    ?DATATYPE -> <<"DATATYPE">>;
    ?WIDTH -> <<"WIDTH">>;
    ?XY -> <<"XY">>;
    ?ENDEL -> <<"ENDEL">>;
    ?SNAME -> <<"SNAME">>;
    ?COLROW -> <<"COLROW">>;
    ?TEXTNODE -> <<"TEXTNODE">>;
    ?NODE -> <<"NODE">>;
    ?TEXTTYPE -> <<"TEXTTYPE">>;
    ?PRESENTATION -> <<"PRESENTATION">>;
    ?STRING -> <<"STRING">>;
    ?STRANS -> <<"STRANS">>;
    ?MAG -> <<"MAG">>;
    ?ANGLE -> <<"ANGLE">>;
    ?REFLIBS -> <<"REFLIBS">>;
    ?FONTS -> <<"FONTS">>;
    ?PATHTYPE -> <<"PATHTYPE">>;
    ?GENERATIONS -> <<"GENERATIONS">>;
    ?ATTRTABLE -> <<"ATTRTABLE">>;
    ?STYPTABLE -> <<"STYPTABLE">>;
    ?STRTYPE -> <<"STRTYPE">>;
    ?ELFLAGS -> <<"ELFLAGS">>;
    ?ELKEY -> <<"ELKEY">>;
    ?NODETYPE -> <<"NODETYPE">>;
    ?PROPATTR -> <<"PROPATTR">>;
    ?PROPVALUE -> <<"PROPVALUE">>;
    ?BOX -> <<"BOX">>;
    ?BOXTYPE -> <<"BOXTYPE">>;
    ?PLEX -> <<"PLEX">>;
    ?BGNEXTN -> <<"BGNEXTN">>;
    ?ENDEXTN -> <<"ENDEXTN">>;
    ?TAPENUM -> <<"TAPENUM">>;
    ?TAPECODE -> <<"TAPECODE">>;
    ?STRCLASS -> <<"STRCLASS">>;
    ?FORMAT -> <<"FORMAT">>;
    ?MASK -> <<"MASK">>;
    ?ENDMASKS -> <<"ENDMASKS">>;
    ?LIBDIRSIZE -> <<"LIBDIRSIZE">>;
    ?SRFNAME -> <<"SRFNAME">>;
    ?LIBSECUR -> <<"LIBSECUR">>;
    ?BORDER -> <<"BORDER">>;
    ?SOFTFENCE -> <<"SOFTFENCE">>;
    ?HARDFENCE -> <<"HARDFENCE">>;
    ?SOFTWIRE -> <<"SOFTWIRE">>;
    ?HARDWIRE -> <<"HARDWIRE">>;
    ?PATHPORT -> <<"PATHPORT">>;
    ?NODEPORT -> <<"NODEPORT">>;
    ?USERCONSTRAINT -> <<"USERCONSTRAINT">>;
    ?SPACERERROR -> <<"SPACERERROR">>;
    ?CONTACT -> <<"CONTACT">>
  end.
