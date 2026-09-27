#!/bin/sh

. ../native/i.sh

for fn in playback playback1u playback1d playback2u playback2d w64mpg; do
  compileFile $fn "" "-lasound -lsndfile -lsamplerate" ""
done

for fn in sender; do
  compileFile $fn "" "-lsndfile -lsamplerate" ""
done

for fn in visMeterLoc visMeterLoc1u visMeterLoc1d visMeterLoc2u visMeterLoc2d; do
  compileFile $fn "" "-lasound -lm" ""
done

for fn in  loopback1d loopback1u loopback2d loopback2u receiver1d receiver1u receiver2d receiver2u; do
  compileFile $fn "" "-lasound" ""
done

for fn in  streamer1d streamer1u streamer2d streamer2u streamerB1d streamerB1u streamerB2d streamerB2u; do
  compileFile $fn "" "-lasound" ""
done

for fn in  streamerL1d streamerL1u streamerL2d streamerL2u streamerR1d streamerR1u streamerR2d streamerR2u; do
  compileFile $fn "" "-lasound" ""
done

for fn in loopback receiver streamer streamerB streamerL streamerR w64play w64rec; do
  compileFile $fn "" "-lasound" ""
done

for fn in visMeterRem; do
  compileFile $fn "" "-lm" ""
done

for fn in  mixer forwarder recorder w64fix; do
  compileFile $fn "" "" ""
done
