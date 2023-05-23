Const
  HexDigits:String[16]='0123456789ABCDEF';

function byte2hextype(b:byte):string;
begin;
byte2hextype:=hexdigits[b shr 4+1]+hexdigits[b and 15+1];
end;

function hextype2byte(s:string):integer;
var i:byte;
begin;
for i:=1 to length(s) do s[i]:=upcase(s[i]);
s:=s+'  ';
hextype2byte:=pos(s[1],hexdigits) shl 4+pos(s[2],hexdigits)-$11;
end;
