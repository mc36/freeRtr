Const
  ASN1objectId_DSSencrypt=#$2A#$86#$48#$CE#$38#$04#$01;
  ASN1objectId_RSAencrypt=#$2A#$86#$48#$86#$F7#$0D#$01#$01#$01;
  ASN1objectId_DSSwithSHA1=#$2A#$86#$48#$CE#$38#$04#$03;
  ASN1objectId_RSAwithSHA1=#$2A#$86#$48#$86#$F7#$0D#$01#$01#$05;
  ASN1objectId_RSAwithMD5=#$2A#$86#$48#$86#$F7#$0D#$01#$01#$04;
  ASN1objectId_RSAwithMD4=#$2A#$86#$48#$86#$F7#$0D#$01#$01#$03;
  ASN1objectId_RSAwithMD2=#$2A#$86#$48#$86#$F7#$0D#$01#$01#$02;
  ASN1objectId_hasherSHA1=#$2B#$0E#$03#$02#$1A;
  ASN1objectId_hasherMD5=#$2A#$86#$48#$86#$F7#$0D#$02#$05;
  ASN1objectId_hasherMD4=#$2A#$86#$48#$86#$F7#$0D#$02#$04;
  ASN1objectId_hasherMD2=#$2A#$86#$48#$86#$F7#$0D#$02#$02;
  ASN1objectId_commonName=#$55#$04#$03;
  ASN1objectId_emailAddress=#$2A#$86#$48#$86#$F7#$0D#$01#$09#$01;
  ASN1objectId_countryName=#$55#$04#$06;
  ASN1objectId_stateOrProvince=#$55#$04#$08;
  ASN1objectId_localityName=#$55#$04#$07;
  ASN1objectId_organization=#$55#$04#$0A;
  ASN1objectId_organizationalUnit=#$55#$04#$0B;




Procedure writeASN1object(var buffer;var ps:LongInt;a:String);
Var buf:array[1..1] of byte absolute buffer;
Begin;
writeASN1header(buf,ps,0,6,length(a));
move(a[1],buf[ps+1],length(a));
inc(ps,length(a));
End;

Procedure writeASN1sequence(var outbuf;var os:LongInt;var inbuf;is:LongInt);
Var buf:array[1..1] of byte absolute outbuf;
Begin;
writeASN1header(buf,os,$20,16,is);
move(inbuf,buf[os+1],is);
inc(os,is);
End;

Procedure writeASN1setof(var outbuf;var os:LongInt;var inbuf;is:LongInt);
Var buf:array[1..1] of byte absolute outbuf;
Begin;
writeASN1header(buf,os,$20,17,is);
move(inbuf,buf[os+1],is);
inc(os,is);
End;

Procedure writeASN1bitstring(var outbuf;var os:LongInt;var inbuf;is:LongInt);
Var buf:array[1..1] of byte absolute outbuf;
Begin;
writeASN1header(buf,os,0,3,is);
move(inbuf,buf[os+1],is);
inc(os,is);
End;

Procedure writeASN1octetstr(var outbuf;var os:LongInt;var inbuf;is:LongInt);
Var buf:array[1..1] of byte absolute outbuf;
Begin;
writeASN1header(buf,os,0,4,is);
move(inbuf,buf[os+1],is);
inc(os,is);
End;

Procedure writeASN1null(var outbuf;var os:LongInt);
Begin;
writeASN1header(outbuf,os,0,5,0);
End;

Procedure addASN1buffer(var outbuf;var os:LongInt;var inbuf;is:LongInt);
Var buf:array[1..1] of byte absolute outbuf;
Begin;
move(inbuf,buf[os+1],is);
inc(os,is);
End;
