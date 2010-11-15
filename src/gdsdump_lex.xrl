Definitions.

Digit = [0-9]
HexDigit = [0-9ABCDEFabcdef]
TagChar = [A-Z]
Space = [\s\t\n\r]

Rules.

{TagChar}+ : {token, {tag, TokenLine, list_to_binary(TokenChars)}}.

-?{Digit}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

-?{Digit}+\.{Digit}+([Ee][+-]?{Digit}+)? : {token, {float, TokenLine, list_to_float(TokenChars)}}.

0x{HexDigit}+ : {token, {integer, TokenLine, list_to_integer(tl(tl(TokenChars)), 16)}}.

\: : {token, {colon, TokenLine}}.

, : {token, {comma, TokenLine}}.

{Space} : skip_token.

".*" : {token, {string, TokenLine, convert_string(TokenChars, TokenLen)}}.

Erlang code.

convert_string(S, L) ->
  RealLength = L - 2,
  case list_to_binary(S) of
    <<_, Cont:RealLength/binary, _>> ->
      Cont
  end.
