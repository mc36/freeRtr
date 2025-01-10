#!/bin/sh
java pathFilter player-all.pls lyr-need.pls $@
java pathRewrite lyr-need.pls /nfs/ /nfs2/
java lyrFetch
