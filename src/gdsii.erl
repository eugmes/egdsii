%% Author: Eugeniy Meshcheryakov <eugen@debian.org>
%% This file is in the public domain.
-module(gdsii).
-export([read_record/1, type_of_tag/1, record_to_binary/1, name_of_tag/1, name_to_tag/1]).

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

int2_from_binary(<<>>) -> [];

int2_from_binary(<<Int2:16/signed, Rest/binary>>) ->
  [Int2 | int2_from_binary(Rest)].

int4_from_binary(<<>>) -> [];

int4_from_binary(<<Int4:32/signed, Rest/binary>>) ->
  [Int4 | int4_from_binary(Rest)].

real8_from_binary(<<>>) -> [];

real8_from_binary(<<Sgn:1, Exp:7, Mant:56, Rest/binary>>) ->
  FSign = case Sgn of
    0 -> 1.0;
    1 -> -1.0
  end,
  % TODO use something like ldexp()
  Float = FSign * Mant * math:pow(2, (4 * (Exp - 64) - 56)),
  [Float | real8_from_binary(Rest)].

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

-spec record_to_binary({non_neg_integer(), record_data()}) -> binary().

record_to_binary({Tag, Data}) ->
  BinData = data_to_binary(Data),
  DataSize = byte_size(BinData) + 4,
  <<DataSize:16, Tag:16, BinData/binary>>.

data_to_binary(nodata) -> <<>>;

data_to_binary({bitarray, Bitarray}) -> <<Bitarray:16>>;

data_to_binary({int2, IntList}) ->
  iolist_to_binary(int2_to_binary_list(IntList));

data_to_binary({int4, IntList}) ->
  iolist_to_binary(int4_to_binary_list(IntList));

data_to_binary({real8, RealList}) ->
  iolist_to_binary(real8_to_binary_list(RealList));

data_to_binary({ascii, S}) ->
  case S of
    Odd when byte_size(Odd) rem 2 == 1 ->
      <<S/binary, 0>>;
    _ -> S
  end.

int2_to_binary_list([I]) -> [<<I:16>>];

int2_to_binary_list([I | Rest]) ->
  [<<I:16>> | int2_to_binary_list(Rest)].

int4_to_binary_list([I]) -> [<<I:32>>];

int4_to_binary_list([I | Rest]) ->
  [<<I:32>> | int4_to_binary_list(Rest)].

real8_to_binary_list([]) -> []; % to save some typing

real8_to_binary_list([R | Rest]) ->
  [real8_to_binary(R) | real8_to_binary_list(Rest)].

real8_to_binary(R) ->
  <<Sign:1, IEEEExp:11, IEEEMant:52>> = <<R/float>>,
  case IEEEExp of
    0 -> <<0:64>>; % denormal
    _ ->
      % substract exponent bias
      UnbIEEEExp = IEEEExp - 1023,
      % add leading one and move to GDSII position
      IEEEMantFull = (IEEEMant + 16#10000000000000) bsl 3,
      % convert exponent to 16-based, +1 for differences in presentation
      % of mantissa (1.xxxx in EEEE and 0.1xxxxx in GDSII
      {Exp16, ExpRest} = divmod(UnbIEEEExp + 1, 4), %FIXME convert to something like Erlang
      % compensate exponent converion
      {NewExp16, NewExpRest} = if
        ExpRest > 0 -> {Exp16 + 1, 4 - ExpRest};
        true -> {Exp16, ExpRest}
      end,
      IEEEMantComp = IEEEMantFull bsr NewExpRest,
      % add GDSII exponent bias
      Exp16Biased = NewExp16 + 64,
      if
        Exp16Biased < -14 -> <<0:64>>;
        Exp16Biased < 0 ->
          M = IEEEMantComp bsr (Exp16Biased * 4),
          <<Sign:1, 0:7, M:56>>;
        Exp16Biased > 16#7f ->
          throw(number_too_big);
        true ->
          <<Sign:1, Exp16Biased:7, IEEEMantComp:56>>
      end
  end.

% TODO remove
divmod(X, Y) ->
  Div = X div Y,
  Mod = X rem Y,
  if
    Mod < 0 -> {Div - 1, Y + Mod};
    true -> {Div, Mod}
  end.

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

-spec name_to_tag(binary()) -> non_neg_integer().

name_to_tag(Name) ->
  case Name of
    <<"HEADER">> -> ?HEADER;
    <<"BGNLIB">> -> ?BGNLIB;
    <<"LIBNAME">> -> ?LIBNAME;
    <<"UNITS">> -> ?UNITS;
    <<"ENDLIB">> -> ?ENDLIB;
    <<"BGNSTR">> -> ?BGNSTR;
    <<"STRNAME">> -> ?STRNAME;
    <<"ENDSTR">> -> ?ENDSTR;
    <<"BOUNDARY">> -> ?BOUNDARY;
    <<"PATH">> -> ?PATH;
    <<"SREF">> -> ?SREF;
    <<"AREF">> -> ?AREF;
    <<"TEXT">> -> ?TEXT;
    <<"LAYER">> -> ?LAYER;
    <<"DATATYPE">> -> ?DATATYPE;
    <<"WIDTH">> -> ?WIDTH;
    <<"XY">> -> ?XY;
    <<"ENDEL">> -> ?ENDEL;
    <<"SNAME">> -> ?SNAME;
    <<"COLROW">> -> ?COLROW;
    <<"TEXTNODE">> -> ?TEXTNODE;
    <<"NODE">> -> ?NODE;
    <<"TEXTTYPE">> -> ?TEXTTYPE;
    <<"PRESENTATION">> -> ?PRESENTATION;
    <<"STRING">> -> ?STRING;
    <<"STRANS">> -> ?STRANS;
    <<"MAG">> -> ?MAG;
    <<"ANGLE">> -> ?ANGLE;
    <<"REFLIBS">> -> ?REFLIBS;
    <<"FONTS">> -> ?FONTS;
    <<"PATHTYPE">> -> ?PATHTYPE;
    <<"GENERATIONS">> -> ?GENERATIONS;
    <<"ATTRTABLE">> -> ?ATTRTABLE;
    <<"STYPTABLE">> -> ?STYPTABLE;
    <<"STRTYPE">> -> ?STRTYPE;
    <<"ELFLAGS">> -> ?ELFLAGS;
    <<"ELKEY">> -> ?ELKEY;
    <<"NODETYPE">> -> ?NODETYPE;
    <<"PROPATTR">> -> ?PROPATTR;
    <<"PROPVALUE">> -> ?PROPVALUE;
    <<"BOX">> -> ?BOX;
    <<"BOXTYPE">> -> ?BOXTYPE;
    <<"PLEX">> -> ?PLEX;
    <<"BGNEXTN">> -> ?BGNEXTN;
    <<"ENDEXTN">> -> ?ENDEXTN;
    <<"TAPENUM">> -> ?TAPENUM;
    <<"TAPECODE">> -> ?TAPECODE;
    <<"STRCLASS">> -> ?STRCLASS;
    <<"FORMAT">> -> ?FORMAT;
    <<"MASK">> -> ?MASK;
    <<"ENDMASKS">> -> ?ENDMASKS;
    <<"LIBDIRSIZE">> -> ?LIBDIRSIZE;
    <<"SRFNAME">> -> ?SRFNAME;
    <<"LIBSECUR">> -> ?LIBSECUR;
    <<"BORDER">> -> ?BORDER;
    <<"SOFTFENCE">> -> ?SOFTFENCE;
    <<"HARDFENCE">> -> ?HARDFENCE;
    <<"SOFTWIRE">> -> ?SOFTWIRE;
    <<"HARDWIRE">> -> ?HARDWIRE;
    <<"PATHPORT">> -> ?PATHPORT;
    <<"NODEPORT">> -> ?NODEPORT;
    <<"USERCONSTRAINT">> -> ?USERCONSTRAINT;
    <<"SPACERERROR">> -> ?SPACERERROR;
    <<"CONTACT">> -> ?CONTACT
  end.
