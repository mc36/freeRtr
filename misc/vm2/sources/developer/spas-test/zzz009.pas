{$stack 512}
{$heap 65500}
{$sysinc system.inc}

Const DataTypeLimit=50;
Type OneDataType=array[1..DataTypeLimit] of integer;
Const oneDataRecord:OneDataType=(
  1979,2370,3632,1910,13,3611,1495,3806,3949,1274,306,1215,2131,3649,395,
  1162,3537,3881,3747,1889,3997,3195,1899,3113,3219,792,1145,2780,2337,
  2331,725,2770,2167,3959,690,2978,3356,1799,1133,206,2612,1863,3467,1317,
  204,2302,1447,1072,2922,946);

Function quickReadOne(n:LongInt):String;
Var a:String;
Begin;
a:=BStr(oneDataRecord[n]);
while (length(a)<8) do a:='0'+a;
quickReadOne:=a;
End;

Procedure quickSwapOne(n1,n2:LongInt);
Var d:longint;
Begin;
d:=oneDataRecord[n1];
oneDataRecord[n1]:=oneDataRecord[n2];
oneDataRecord[n2]:=d;
End;


{$sysinc quicksrt.inc}



Procedure PrintList;
Var
  i:LongInt;
  a:String;
Begin;
for i:=1 to DataTypeLimit do begin;
  a:=bstr(oneDataRecord[i]);
  while (length(a)<8) do a:=' '+a;
  Write(a);
  end;
end;

Var i:LongInt;
BEGIN;
WriteLn('original:');
PrintList;
Write('sorting...');
quickSort(1,DataTypeLimit);
Writeln(' done.');
WriteLn('sorted:');
PrintList;
i:=QuickFind(1,DataTypeLimit,'00001979');
WriteLn(BStr(i)+': '+bstr(oneDataRecord[i]));
END.