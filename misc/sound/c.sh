#!/bin/sh

. ../native/i.sh

for fn in playback playback1u playback1d w64mpg; do
  compileFile $fn "" "-lasound -lsndfile -lsamplerate" ""
done

for fn in sender; do
  compileFile $fn "" "-lsndfile -lsamplerate" ""
done

for fn in visMeterLoc visMeterLoc1u visMeterLoc1d; do
  compileFile $fn "" "-lasound -lm" ""
done

for fn in receiver receiver1d receiver1u streamer streamer1d streamer1u loopback loopback1d loopback1u streamerBth streamerBth1d streamerBth1u streamerLft streamerLft1d streamerLft1u streamerRgt streamerRgt1d streamerRgt1u w64play w64rec; do
  compileFile $fn "" "-lasound" ""
done

for fn in visMeterRem; do
  compileFile $fn "" "-lm" ""
done

for fn in  mixer forwarder recorder w64fix; do
  compileFile $fn "" "" ""
done
