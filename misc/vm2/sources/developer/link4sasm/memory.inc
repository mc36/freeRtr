Type
  OneRecordType=record
    Typ:Byte;           {type: 1=label 2=const 3=ofs 4=num}
    Nam:String[83];     {name of object}
    Len:Byte;           {length of object to put}
    Sig:Boolean;        {true=signed false=unsigned}
    Ord:Boolean;        {false=lsb true=msb}
    Rng:Boolean;        {range check needed}
    sLab:Boolean;       {name can be label}
    sCnt:Boolean;       {name can be const}
    Rel2:Byte;          {relative to: 0=none, 1=beg, 2=end}
    vAdd:LongInt;       {value to add to}
    vAnd:LongInt;       {value to and with}
    vOr:LongInt;        {value to or with}
    vMul:LongInt;       {multiply with}
    vDiv:LongInt;       {divide with}
    vMod:LongInt;       {remainder with}
    vShr:LongInt;       {rotate right}
    vShl:LongInt;       {rotate left}
    Lin:LongInt;        {object file line number}
    Pos:LongInt;        {binary file position}
    end;

Type
  TheExtMemBufRecType=array[1..16] of OneRecordType;
  TheExtMemBufBinType=array[1..1024] of byte;
Var
  TheExtMemBufRecDat:^TheExtMemBufRecType;
  TheExtMemBufBinDat:^TheExtMemBufBinType;
  recordUsed:LongInt;
  recordAllc:LongInt;
  recordMark:LongInt;
  fileHandler:xFile;
  binaryBegn:LongInt;
  binaryVald:LongInt;
  binarySize:LongInt;
  binaryAllc:LongInt;
  binaryPosi:LongInt;


Procedure resizeMem(o:LongInt);
Var
  i:LongInt;
  p:Pointer;
Begin;
inc(o,1024);
i:=ExtendedMemoryResize(p,o);
if (i<o) then WriteErr('out of memory! ('+BStr(o div 1024)+'k needed)',0);
TheExtMemBufRecDat:=p^;
TheExtMemBufBinDat:=p^;
End;

Procedure allocateMem;
Var i,o:LongInt;
Begin;
o:=recordAllc*sizeof(OneRecordType);
resizeMem(o+binaryAllc);
if (binaryBegn=o) then exit;
move(TheExtMemBufBinDat^[binaryBegn+1],TheExtMemBufBinDat^[o+1],binarySize);
binaryBegn:=o;
End;

Procedure readRec(n:LongInt;Var d:OneRecordType);
Begin;
d:=TheExtMemBufRecDat^[n];
End;

Procedure writeRec(n:LongInt;Var d:OneRecordType);
Begin;
TheExtMemBufRecDat^[n]:=d;
End;

Procedure appendRec(Var d:OneRecordType);
Var i,o:LongInt;
Begin;
inc(recordUsed);
if (recordUsed>recordAllc) then begin;
  recordAllc:=recordUsed+256;
  allocateMem;
  end;
TheExtMemBufRecDat^[recordUsed]:=d;
End;

Function quickReadOne(n:LongInt):String;
Var d:OneRecordType;
Begin;
readRec(n,d);
quickReadOne:=d.Nam;
End;

Procedure quickSwapOne(n1,n2:LongInt);
Var d1,d2:OneRecordType;
Begin;
readRec(n1,d1);
readRec(n2,d2);
writeRec(n2,d1);
writeRec(n1,d2);
End;
