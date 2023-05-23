{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}
{$sysinc inet_cgi.inc}
{$sysinc inet_addr.inc}
Const
  ProggyName='Visitor Lister v1.0/CGi';


function x(i:longint):string;
var a:string;
begin;
a:=bstr(i);
while (length(a)<2) do a:='0'+a;
x:=a;
end;

Var
  vis:record
    year:word;month:word;day:Word;
    hour:word;min:word;sec:Word;
    ipaddr:array[1..16] of char;
    agent:String;
    filler:array[1..228] of char;
    end;
  f:xFile;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);

if (xOpen(f,'visitor.data',xGenFilMod_r)<>0) then halt(3);
WriteResponseHeader(3,'html');
WriteLnResponseString('<html><head><title>visitors</title></head><body>');
WriteLnResponseString('<p align="center"><font size="+3"><b><i><u>visitors</u></i></b></font></p>');
WriteLnResponseString('<table border=2 cellspacing=2 cellpadding=0>');
WriteLnResponseString('<tr>');
WriteLnResponseString('  <td><b>date</b></td>');
WriteLnResponseString('  <td><b>address</b></td>');
WriteLnResponseString('  <td><b>browser</b></td>');
WriteLnResponseString('  </tr>');

for p:=1 to xFileSize(f) div sizeof(vis) do begin;
  xBlockRead(f,vis,sizeof(vis));
  WriteLnResponseString('<tr>');
  WriteLnResponseString('  <td>'+x(vis.year)+'-'+x(vis.month)+'-'+x(vis.day)+' '+x(vis.hour)+':'+x(vis.min)+':'+x(vis.sec)+'</td>');
  WriteLnResponseString('  <td>'+ipAddr2string(vis.ipaddr)+'</td>');
  WriteLnResponseString('  <td>'+vis.agent+'</td>');
  WriteLnResponseString('  </tr>');
  end;

WriteLnResponseString('</table>');
WriteLnResponseString('<br>');
WriteLnResponseString('<i>Created by '+ProggyName+'</i>');
WriteLnResponseString('</body>');
WriteLnResponseString('</html>');
CloseRequestFile;
END.