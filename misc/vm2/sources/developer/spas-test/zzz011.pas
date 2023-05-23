{$stack 512}
{$heap 65500}
{$sysinc system.inc}
Type
  OneDrawCharRec=record
    c:char;
    u:byte;
    d:byte;
    l:byte;
    r:byte;
    end;
Const
  DrawChrs:array[0..40] of OneDrawCharRec=(   (c:' ';u:0;d:0;l:0;r:0),
  (c:'Ä';u:0;d:0;l:1;r:1),(c:'Í';u:0;d:0;l:2;r:2),
  (c:'³';u:1;d:1;l:0;r:0),(c:'º';u:2;d:2;l:0;r:0),
  (c:'Ñ';u:0;d:1;l:2;r:2),(c:'Ò';u:0;d:2;l:1;r:1),
  (c:'Ø';u:1;d:1;l:2;r:2),(c:'×';u:2;d:2;l:1;r:1),
  (c:'Ï';u:1;d:0;l:2;r:2),(c:'Ð';u:2;d:0;l:1;r:1),
  (c:'Â';u:0;d:1;l:1;r:1),(c:'Ë';u:0;d:2;l:2;r:2),
  (c:'Å';u:1;d:1;l:1;r:1),(c:'Î';u:2;d:2;l:2;r:2),
  (c:'Á';u:1;d:0;l:1;r:1),(c:'Ê';u:2;d:0;l:2;r:2),
  (c:'¿';u:0;d:1;l:1;r:0),(c:'»';u:0;d:2;l:2;r:0),
  (c:'´';u:1;d:1;l:1;r:0),(c:'¹';u:2;d:2;l:2;r:0),
  (c:'Ù';u:1;d:0;l:1;r:0),(c:'¼';u:2;d:0;l:2;r:0),
  (c:'Ú';u:0;d:1;l:0;r:1),(c:'É';u:0;d:2;l:0;r:2),
  (c:'Ã';u:1;d:1;l:0;r:1),(c:'Ì';u:2;d:2;l:0;r:2),
  (c:'À';u:1;d:0;l:0;r:1),(c:'È';u:2;d:0;l:0;r:2),
  (c:'¸';u:0;d:1;l:2;r:0),(c:'·';u:0;d:2;l:1;r:0),
  (c:'µ';u:1;d:1;l:2;r:0),(c:'¶';u:2;d:2;l:1;r:0),
  (c:'¾';u:1;d:0;l:2;r:0),(c:'½';u:2;d:0;l:1;r:0),
  (c:'Õ';u:0;d:1;l:0;r:2),(c:'Ö';u:0;d:2;l:0;r:1),
  (c:'Æ';u:1;d:1;l:0;r:2),(c:'Ç';u:2;d:2;l:0;r:1),
  (c:'Ô';u:1;d:0;l:0;r:2),(c:'Ó';u:2;d:0;l:0;r:1)
  );

Function GetOneChar(x,y:LongInt):Byte;
Begin;
GetOneChar:=x+y;
End;

Var
  ox,oy:LongInt;
  dat:OneDrawCharRec;
BEGIN;
ox:=4;
oy:=3;
dat.l:=DrawChrs[GetOneChar(ox-1,oy)].r;
dat.r:=DrawChrs[GetOneChar(ox+1,oy)].l;
dat.u:=DrawChrs[GetOneChar(ox,oy-1)].d;
dat.d:=DrawChrs[GetOneChar(ox,oy+1)].u;
writeln(BStr(dat.l));
writeln(BStr(dat.r));
writeln(BStr(dat.u));
writeln(BStr(dat.d));
END.