type
  ISOtype711=array[1..1] of byte;
  ISOtype712=array[1..2] of byte;
  ISOtype721=array[1..2] of byte;
  ISOtype722=array[1..2] of byte;
  ISOtype723=array[1..4] of byte;
  ISOtype731=array[1..4] of byte;
  ISOtype732=array[1..4] of byte;
  ISOtype733=array[1..8] of byte;
type
  iso_primary_descriptor=record
    typ:ISOtype711;
    id:array[1..5] of byte;
    version:ISOtype711;
    unused1:array[1..1] of byte;
    system_id:array[1..32] of byte;
    volume_id:array[1..32] of byte;
    unused2:array[1..8] of byte;
    volume_space_size:ISOtype733;
    unused3:array[1..32] of byte;
    volume_set_size:ISOtype723;
    volume_sequence_number:ISOtype723;
    logical_block_size:ISOtype723;
    path_table_size:ISOtype733;
    type_l_path_table:ISOtype731;
    opt_type_l_path_table:ISOtype731;
    type_m_path_table:ISOtype732;
    opt_type_m_path_table:ISOtype732;
    root_directory_record:array[1..34] of byte;
    volume_set_id:array[1..128] of byte;
    publisher_id:array[1..128] of byte;
    preparer_id:array[1..128] of byte;
    application_id:array[1..128] of byte;
    copyright_file_id:array[1..37] of byte;
    abstract_file_id:array[1..37] of byte;
    bibliographic_file_id:array[1..37] of byte;
    creation_date:array[1..17] of byte;
    modification_date:array[1..17] of byte;
    expiration_date:array[1..17] of byte;
    effective_date:array[1..17] of byte;
    file_structure_version:ISOtype711;
    unused4:array[1..1] of byte;
    application_data:array[1..512] of byte;
    end;
  iso_directory_record=record
    length:ISOtype711;
    ext_attr_length:ISOtype711;
    extent:ISOtype733;
    size:ISOtype733;
    date_year:ISOtype711;
    date_month:ISOtype711;
    date_day:ISOtype711;
    date_hour:ISOtype711;
    date_minute:ISOtype711;
    date_second:ISOtype711;
    date_gmt:ISOtype711;
    flags:ISOtype711;
    file_unit_size:ISOtype711;
    interleave:ISOtype711;
    volume_sequence_number:ISOtype723;
    name_len:ISOtype711;
    {name}
    end;


function ISOnum711(var buf:ISOtype711):Byte;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR lubub a [src];
asm movW dubub [trg] a;
end;

function ISOnum712(var buf:ISOtype712):Byte;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR lubub a [src];
asm movW dubub [trg] a;
end;

function ISOnum721(var buf:ISOtype721):Word;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR luwuw a [src];
asm movW duwuw [trg] a;
end;

function ISOnum722(var buf:ISOtype722):Word;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR muwuw a [src];
asm movW duwuw [trg] a;
end;

function ISOnum723(var buf:ISOtype723):Word;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR luwuw a [src];
asm movW duwuw [trg] a;
end;

function ISOnum731(var buf:ISOtype731):LongInt;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR ludud a [src];
asm movW dudud [trg] a;
end;

function ISOnum732(var buf:ISOtype732):LongInt;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR mudud a [src];
asm movW dudud [trg] a;
end;

function ISOnum733(var buf:ISOtype733):LongInt;
begin;
asm varAddr src buf;
asm addrLod src [src];
asm varAddr trg @result;
asm movR ludud a [src];
asm movW dudud [trg] a;
end;

function ISOstring(var buf;len:Word):String;
const max=254;
var a:string;
begin;
if (len>max) then len:=max;
move(buf,a[1],len);
a[0]:=chr(len);
len:=pos(#0,a);
if (len>0) then a:=copy(a,1,len-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
ISOstring:=a;
end;
