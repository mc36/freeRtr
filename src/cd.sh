#!/bin/sh

scanOne()
{
echo "scanning for $1 as $2."
find org/ -type f -exec grep -l -P "$2" {} \;
}


echo "scanning for duplicate features"
cat rtr.ftr | sort | uniq -d
scanOne "trailing space" " +$"
scanOne "username" `whoami`
scanOne "here" "\"here"
scanOne "non-ascii" "[^\x00-\x7F]"
scanOne "override" "\@Override"
scanOne "exception" "throw new"
scanOne "this" "this\."
scanOne "for-each" " for .* : "
scanOne "diamond" "\<\>"
scanOne "finally" "\ finally\ "
scanOne "new thread" "new\ Thread\("
scanOne "var" " var "
scanOne "case-str" "case \""
scanOne "stat-fin" "static\ final"
scanOne "stat-imp" "import\ static"
scanOne "null-and" "\=\ null\ \&"
scanOne "null-or" "\=\ null\ \|"
scanOne "pre-inc" "\ \+\+[a-z|A-Z]"
scanOne "pre-dec" "\ \-\-[a-z|A-Z]"
scanOne "no-brck" " - 1; .\+\+"
scanOne ".tostring()" "\.toString\(\)"
scanOne "///" "\/\/\/"
echo "generating javadoc"
find . -name "*.java" -print0 | xargs -s 512000 -0 javadoc -source 11 -use -linksource -keywords -quiet -d ../binTmp/
#doxygen ../misc/doxygen.cfg
