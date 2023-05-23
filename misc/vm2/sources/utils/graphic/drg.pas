{$include filter1.pas}
Const
  proggyName='drg v1.0';
  proggyCapa=3;


Type
  oneImageHeaderRecord=record
    typ:LongInt;
    imgX:LongInt;
    imgY:LongInt;
    end;


Function doReadup:Boolean;
Var
  buf:array[1..maxScreenWidth] of byte;
  hdr:oneImageHeaderRecord absolute buf;
  sx,sy,i,o,p:LongInt;
Begin;
doReadup:=true;
uppReadFile(0,sizeof(hdr),hdr);
if (ReadLongLSB(hdr.typ)<>101) then exit;
sx:=ReadLongLSB(hdr.imgX);
sy:=ReadLongLSB(hdr.imgY);
if (sy*sx+sizeof(hdr)+768<>uppFileSize) then exit;
uppReadFile(sizeof(hdr),256*3,buf);
for i:=1 to 256*3 do buf[i]:=buf[i] shl 2;
uppPutPalette(sx,sy,buf);
p:=sizeof(hdr)+256*3;
for o:=1 to sy do begin;
  uppReadFile(p,sx,buf);
  inc(p,sx);
  uppPutLine(0,o-1,sx,buf);
  end;
doReadup:=false;
End;


Procedure doWriter;
Var
  buf:array[1..maxScreenWidth] of byte;
  hdr2:array[1..512] of byte;
  hdr:oneImageHeaderRecord absolute hdr2;
  sx,sy,i,o,p:LongInt;
Begin;
uppGetPalette(sx,sy,buf);
for i:=1 to 256*3 do buf[i]:=buf[i] shr 2;
WriteLongLSB(hdr.typ,101);
WriteLongLSB(hdr.imgX,sx);
WriteLongLSB(hdr.imgY,sy);
uppWriteFile(0,sizeof(hdr),hdr);
uppWriteFile(sizeof(hdr),256*3,buf);
p:=sizeof(hdr)+256*3;
for o:=1 to sy do begin;
  uppGetLine(0,o-1,sx,buf);
  uppWriteFile(p,sx,buf);
  inc(p,sx);
  end;
End;

{$include filter2.pas}BEGIN;doMain;END.