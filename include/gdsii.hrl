% Tag types
-define(NODATA, 0).
-define(BITARRAY, 1).
-define(INT2, 2).
-define(INT4, 3).
-define(REAL4, 4). % not used
-define(REAL8, 5).
-define(ASCII, 6).

% Tag types
-define(HEADER, 16#0002).
-define(BGNLIB, 16#0102).
-define(LIBNAME, 16#0206).
-define(UNITS, 16#0305).
-define(ENDLIB, 16#0400).
-define(BGNSTR, 16#0502).
-define(STRNAME, 16#0606).
-define(ENDSTR, 16#0700).
-define(BOUNDARY, 16#0800).
-define(PATH, 16#0900).
-define(SREF, 16#0A00).
-define(AREF, 16#0B00).
-define(TEXT, 16#0C00).
-define(LAYER, 16#0D02).
-define(DATATYPE, 16#0E02).
-define(WIDTH, 16#0F03).
-define(XY, 16#1003).
-define(ENDEL, 16#1100).
-define(SNAME, 16#1206).
-define(COLROW, 16#1302).
-define(TEXTNODE, 16#1400).
-define(NODE, 16#1500).
-define(TEXTTYPE, 16#1602).
-define(PRESENTATION, 16#1701).
% SPACING, 16#18??
-define(STRING, 16#1906).
-define(STRANS, 16#1A01).
-define(MAG, 16#1B05).
-define(ANGLE, 16#1C05).
% UINTEGER, 16#1D??
% USTRING, 16#1E??
-define(REFLIBS, 16#1F06).
-define(FONTS, 16#2006).
-define(PATHTYPE, 16#2102).
-define(GENERATIONS, 16#2202).
-define(ATTRTABLE, 16#2306).
-define(STYPTABLE, 16#2406).
-define(STRTYPE, 16#2502).
-define(ELFLAGS, 16#2601).
-define(ELKEY, 16#2703).
% LINKTYPE, 16#28??
% LINKKEYS, 16#29??
-define(NODETYPE, 16#2A02).
-define(PROPATTR, 16#2B02).
-define(PROPVALUE, 16#2C06).
-define(BOX, 16#2D00).
-define(BOXTYPE, 16#2E02).
-define(PLEX, 16#2F03).
-define(BGNEXTN, 16#3003).
-define(ENDEXTN, 16#3103).
-define(TAPENUM, 16#3202).
-define(TAPECODE, 16#3302).
-define(STRCLASS, 16#3401).
% RESERVED, 16#3503
-define(FORMAT, 16#3602).
-define(MASK, 16#3706).
-define(ENDMASKS, 16#3800).
-define(LIBDIRSIZE, 16#3902).
-define(SRFNAME, 16#3A06).
-define(LIBSECUR, 16#3B02).
% Types used only with Custom Plus
-define(BORDER, 16#3C00).
-define(SOFTFENCE, 16#3D00).
-define(HARDFENCE, 16#3E00).
-define(SOFTWIRE, 16#3F00).
-define(HARDWIRE, 16#4000).
-define(PATHPORT, 16#4100).
-define(NODEPORT, 16#4200).
-define(USERCONSTRAINT, 16#4300).
-define(SPACERERROR, 16#4400).
-define(CONTACT, 16#4500).
