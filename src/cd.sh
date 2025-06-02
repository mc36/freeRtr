#!/bin/sh

scanOne()
{
echo "scanning for $1 as $2."
find . -type f -exec grep -l -P "$2" {} \;
}


echo "scanning for duplicate features"
cat rtr.ftr | sort | uniq -d
scanOne "trailing space" " +$"
scanOne "username" `whoami`
scanOne "non-ascii" "[^\x00-\x7F]"
scanOne "override" "\@Override"
scanOne "exception" "throw new"
scanOne "this" "this\."
scanOne "here" "\"here"
scanOne "stat-fin" "static\ final"
scanOne "stat-imp" "import\ static"
scanOne "pre-inc" "\ \+\+[a-z|A-Z]"
scanOne "pre-dec" "\ \-\-[a-z|A-Z]"
scanOne ".tostring()" "\.toString\(\)"
scanOne "///" "\/\/\/"
echo "generating javadoc"
find . -name "*.java" -print0 | xargs -s 512000 -0 javadoc -source 11 -use -linksource -keywords -quiet -d ../binTmp/
#doxygen ../misc/doxygen.cfg
