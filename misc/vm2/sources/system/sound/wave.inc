Type
  OneWaveIdentRecord=Array[1..4] of Char;
  OneWaveHeaderRecord=record
    RIFFid:OneWaveIdentRecord;  {'RIFF'}
    RIFFsize:LongInt;           {filesize-8}
    WAVEid:OneWaveIdentRecord;  {'WAVE'}
    FRMTid:OneWaveIdentRecord;  {'fmt '}
    FRMTsize:LongInt;           {0x10}
    FRMTtype:Word;              {1}
    Channels:Word;              {1-mono, 2-stereo}
    Frequency:LongInt;          {frequency: 11khz, 22khz, 44khz, etc}
    BytesPerSec:LongInt;        {freq*BytePerSample}
    BytePerSample:Word;         {chns*bitPerSam/8}
    BitPerSample:Word;          {BitPerSample}
    DATAid:OneWaveIdentRecord;  {'data'}
    DATAsize:LongInt;           {filesize-44}
    end;


Function ConvWaveIdent2Str(d:OneWaveIdentRecord):String;
Var a:String;
Begin;
Move(d,a[1],SizeOf(d));
a[0]:=Chr(SizeOf(d));
ConvWaveIdent2Str:=a;
End;

Procedure ConvStr2WaveIdent(a:String;Var d:OneWaveIdentRecord);
Begin;
Move(a[1],d,SizeOf(d));
End;

Procedure RepairWaveHeader(Var f:xFile;Freq:Word;Str,Wrd:Boolean);
Var
  h:OneWaveHeaderRecord;
  CurrSize:LongInt;
  i:LongInt;
Begin;
CurrSize:=xFileSize(f);
FillChar(h,SizeOf(h),0);
ConvStr2WaveIdent('RIFF',h.RIFFid);
h.RIFFsize:=CurrSize-8;
ConvStr2WaveIdent('WAVE',h.WAVEid);
ConvStr2WaveIdent('fmt ',h.FRMTid);
h.FRMTsize:=$10;
h.FRMTtype:=1;
if Str then i:=2 else i:=1;
h.Channels:=i;
h.Frequency:=Freq;
if Wrd then i:=16 else i:=8;
h.BitPerSample:=i;
h.BytePerSample:=h.Channels*h.BitPerSample div 8;
h.BytesPerSec:=h.BytePerSample*h.Frequency;
ConvStr2WaveIdent('data',h.DATAid);
h.DATAsize:=CurrSize-44;
xSeek(f,0);
xBlockWrite(f,h,SizeOf(h));
End;

Function TestWaveHeader(Var f:xFile;Var Freq:Word;Var Str,Wrd:Boolean):Boolean;
Var
  h:OneWaveHeaderRecord;
  CurrSize:LongInt;
Begin;
TestWaveHeader:=True;
xSeek(f,0);
xBlockRead(f,h,SizeOf(h));
CurrSize:=xFileSize(f);
if (ConvWaveIdent2Str(h.RIFFid)<>'RIFF') then Exit;
if (h.RIFFsize<>CurrSize-8) then Exit;
if (ConvWaveIdent2Str(h.WAVEid)<>'WAVE') then Exit;
if (ConvWaveIdent2Str(h.FRMTid)<>'fmt ') then Exit;
if (h.FRMTsize<>$10) then Exit;
if (h.FRMTtype<>1) then Exit;
if (not h.Channels in [1,2]) then Exit;
Str:=(h.Channels=2);
if (h.Frequency<4000) or (h.Frequency>65535) then Exit;
Freq:=h.Frequency;
if (not h.BitPerSample in [8,16]) then Exit;
Wrd:=(h.BitPerSample=16);
if (h.BytePerSample<>h.Channels*h.BitPerSample div 8) then Exit;
if (h.BytesPerSec<>h.BytePerSample*h.Frequency) then Exit;
if (ConvWaveIdent2Str(h.DATAid)<>'data') then Exit;
if (h.DATAsize<>CurrSize-44) then Exit;
TestWaveHeader:=False;
End;
