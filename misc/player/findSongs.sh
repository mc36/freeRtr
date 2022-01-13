#!/bin/sh
java findSongs /nfs/temp player-new.pls player-new.pls found1.pls
java findSongs /nfs/musica player-cls.pls player-cls.pls found2.pls
java findSongs /nfs/music player-all.pls player-all.pls found3.pls
cat found1.pls found2.pls found3.pls > lyr-needed.pls
rm found1.pls found2.pls found3.pls
cat lyr-needed.pls >> player-new.pls
sed 's+=/nfs/+=/nfs2/+g' lyr-needed.pls > lyr-need.pls
java lyrFetch
