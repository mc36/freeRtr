Type MemoryDataRecordType=array[1..16] of String;
Var
  MemoryDataRecord:^MemoryDataRecordType;
  MemoryLines:LongInt;
  MemoryChanged:Boolean;

Function dup(n:Byte;c:Char):String;
Var
  a:string;
  i:word;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;


Procedure WriteLine(n:LongInt;a:String);
Begin;
if (n<1) or (n>MemoryLines) then Exit;
a:=copy(a,1,250);
while (copy(a,length(a),1)=' ') do a:=copy(a,1,length(a)-1);
MemoryDataRecord^[n]:=a;
MemoryChanged:=True;
End;

Function ReadLine(n:LongInt):String;
Begin;
if (n<1) or (n>MemoryLines) then begin;
  ReadLine:='';
  Exit;
  end;
ReadLine:=MemoryDataRecord^[n];
End;

Function ResizeMemory(n:LongInt):Boolean;
Var
  p:Pointer;
  o:LongInt;
Begin;
ResizeMemory:=True;
o:=n*sizeof(MemoryDataRecord^[1])+1024;
if (ExtendedMemoryResize(p,o)<o) then exit;
MemoryDataRecord:=p^;
MemoryLines:=n;
MemoryChanged:=True;
ResizeMemory:=False;
End;

Function InsertLine(n:LongInt):Boolean;
Begin;
InsertLine:=True;
if (n<1) or (n>MemoryLines+1) then exit;
if ResizeMemory(MemoryLines+1) then exit;
move(MemoryDataRecord^[n],MemoryDataRecord^[n+1],(MemoryLines-n)*sizeof(MemoryDataRecord^[1]));
MemoryDataRecord^[n]:='';
MemoryChanged:=True;
InsertLine:=False;
End;

Function DeleteLine(n:LongInt):Boolean;
Label vege;
Begin;
DeleteLine:=True;
if (n<1) or (n>MemoryLines) then Exit;
if (MemoryLines<2) then begin;
  ResizeMemory(1);
  MemoryDataRecord^[1]:='';
  goto vege;
  end;
move(MemoryDataRecord^[n+1],MemoryDataRecord^[n],(MemoryLines-n)*sizeof(MemoryDataRecord^[1]));
if ResizeMemory(MemoryLines-1) then exit;
vege:
MemoryChanged:=True;
DeleteLine:=False;
End;
