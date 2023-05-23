{$include filter1.pas}
Const
  proggyName='windows bitmap v1.0';
  proggyCapa=3;


Type
  oneImageHeaderRecord=record
    typ:Word;
    filSiz:LongInt;
    res1:Word;
    res2:Word;
    byteOff:LongInt;
    hdrSiz:LongInt;
    imgXsiz:LongInt;
    imgYsiz:LongInt;
    plane:Word;
    bitPix:Word;
    comr:LongInt;
    imgSiz:LongInt;
    resX:LongInt;
    resY:LongInt;
    colorU:LongInt;
    colorI:LongInt;
    end;


Function doReadup:Boolean;
Var
  buf:array[1..maxScreenWidth] of byte;
  hdr:oneImageHeaderRecord absolute buf;
  sx,sy,i,o,p:LongInt;
Begin;
doReadup:=true;
uppReadFile(0,sizeof(hdr),hdr);
if (readWordLSB(hdr.typ)<>19778) then exit;
if (readLongLSB(hdr.comr)<>0) then exit;
if (readWordLSB(hdr.bitPix)<>8) then exit;
sx:=readLongLSB(hdr.imgXsiz);
sy:=readLongLSB(hdr.imgYsiz);

uppReadFile(sizeof(hdr),1024,buf[1025]);
o:=1024;
p:=0;
for i:=0 to 255 do begin;
  buf[p+1]:=buf[o+3];
  buf[p+2]:=buf[o+2];
  buf[p+3]:=buf[o+1];
  inc(p,3);
  inc(o,4);
  end;
uppPutPalette(sx,sy,buf);
p:=sizeof(hdr)+1024;
i:=(sx+3) and $fffffc;
for o:=sy downto 1 do begin;
  uppReadFile(p,sx,buf);
  inc(p,i);
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
fillchar(hdr,sizeof(hdr),0);
uppGetPalette(sx,sy,buf[1025]);
o:=3*255+1024;
p:=4*255;
for i:=0 to 255 do begin;
  buf[p+1]:=buf[o+3];
  buf[p+2]:=buf[o+2];
  buf[p+3]:=buf[o+1];
  buf[p+4]:=0;
  dec(o,3);
  dec(p,4);
  end;
p:=sizeof(hdr)+1024;
i:=(sx+3) and $fffffc;
WriteWordLSB(hdr.typ,19778);
WriteLongLSB(hdr.byteOff,1078);
WriteLongLSB(hdr.imgSiz,sy*i);
WriteLongLSB(hdr.filSiz,hdr.imgSiz+hdr.byteOff);
WriteLongLSB(hdr.hdrSiz,40);
WriteLongLSB(hdr.imgXsiz,sx);
WriteLongLSB(hdr.imgYsiz,sy);
WriteWordLSB(hdr.plane,1);
WriteWordLSB(hdr.bitPix,8);
uppWriteFile(0,sizeof(hdr),hdr);
uppWriteFile(sizeof(hdr),1024,buf);
for o:=sy downto 1 do begin;
  uppGetLine(0,o-1,sx,buf);
  fillchar(buf[sx+1],i-sx,0);
  uppWriteFile(p,i,buf);
  inc(p,i);
  end;
End;

{$include filter2.pas}BEGIN;doMain;END.