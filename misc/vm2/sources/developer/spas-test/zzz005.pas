{$stack 512}
{$heap 65500}
{$sysinc system.inc}
Const MatrixMax=5;
Type OneMatrixRec=array[1..MatrixMax,1..MatrixMax] of integer;
Const DefaultMatrix:OneMatrixRec=(  (1,2,3,4,5),(2,3,4,5,6),(3,4,5,6,7),(4,5,6,7,8),(5,6,7,8,9)  );

Var
  DataDef:OneMatrixRec;
  DataAbs:OneMatrixRec absolute DataDef;
  DataRel:^OneMatrixRec;
  i,o:Word;
BEGIN;
DataRel:=@dataAbs;
dataDef:=DefaultMatrix;
for o:=1 to MatrixMax do for i:=1 to MatrixMax do DataAbs[o][i]:=DataRel^[o][i]-1;
for o:=1 to MatrixMax do for i:=1 to MatrixMax do DataRel^[o][i]:=2*DataAbs[o][i];
for o:=1 to MatrixMax do begin;
  for i:=1 to MatrixMax do Write(' '+BStr(DataRel^[o][i]));
  WriteLn('');
  end;
END.