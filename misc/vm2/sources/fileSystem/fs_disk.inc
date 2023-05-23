{uncomment _one_ of these depending on media you want!}

{{$include disk1.inc}            {disk in file schema...}
{$include disk2.inc}            {direct disk access...}
{{$include disk3.inc}            {int13h disk access...}



{uncomment _one_ of these depending on algorithm you want!}

{{$include cache1.inc}           {caching disabled...}
{$include cache2.inc}           {16k read/write caching...}
