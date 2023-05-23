{$include filter1.pas}
{$sysinc filesys.inc}
Const
  proggyName='dra v1.0';
  proggyCapa=1;


Type
  oneImageHeaderRecord=record
    imgX:Word;
    imgY:Word;
    end;

Procedure readOneFile(a:String;m:LongInt;var buf);
Var
  f:xFile;
  i:LongInt;
Begin;
fillchar(buf,m,0);
if (xOpen(f,'\system\graphic\'+a,xGenFilMod_r)<>0) then exit;
i:=xFileSize(f);
if (i>m) then i:=m;
xBlockRead(f,buf,i);
xClose(f);
End;

Function doReadup:Boolean;
Var
  font:array[0..255,1..16] of byte;
  pal:array[1..256*3] of byte;
  bfi:array[1..maxScreenWidth div 4] of byte;
  buf:array[1..maxScreenWidth] of byte;
  hdr:oneImageHeaderRecord absolute buf;
  sx,sy,i,o,p:LongInt;

Procedure getLn(ln:LongInt);
Var
  i,o,p,q,ch,co:LongInt;
Begin;
p:=0;
for q:=1 to sx do begin;
  ch:=font[bfi[(q shl 1)-1]][ln];
  co:=bfi[q shl 1];
  for o:=1 to 8 do begin;
    if (ch and $80<>0) then i:=co and $f else i:=co shr 4;
    inc(p);
    buf[p]:=i;
    ch:=ch shl 1;
    end;
  end;
End;

Begin;
doReadup:=true;
uppReadFile(0,sizeof(hdr),hdr);
sx:=readWordLSB(hdr.imgX);
sy:=readWordLSB(hdr.imgY);
if (sy*sx*2+sizeof(hdr)<>uppFileSize) then exit;
readOneFile('font8x16.data',sizeof(font),font);
readOneFile('pal16.data',sizeof(pal),pal);
move(pal[169],pal[25],24);
fillchar(pal[49],744,0);
for i:=1 to 48 do pal[i]:=pal[i] shl 2;
uppPutPalette(sx*8,sy*16,pal);
for o:=1 to sy do begin;
  uppReadFile((o-1)*sx*2+sizeof(hdr),sx*2,bfi);
  for i:=1 to 16 do begin;
    getLn(i);
    uppPutLine(0,o*16+i-17,sx*8,buf);
    end;
  end;
doReadup:=false;
End;


Procedure doWriter;
Begin;
End;

{$include filter2.pas}BEGIN;doMain;END.