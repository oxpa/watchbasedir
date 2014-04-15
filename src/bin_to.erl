-module(bin_to).
-export([hex/1]).

hex(D) when is_binary(D) ->
    binary:bin_to_list(<< <<(to16(X))/integer>> || <<X:4>> <= D >>).
 
to16(0) -> $0;
to16(1) -> $1;
to16(2) -> $2;
to16(3) -> $3;
to16(4) -> $4;
to16(5) -> $5;
to16(6) -> $6;
to16(7) -> $7;
to16(8) -> $8;
to16(9) -> $9;
to16(10) -> $a;
to16(11) -> $b;
to16(12) -> $c;
to16(13) -> $d;
to16(14) -> $e;
to16(15) -> $f.
