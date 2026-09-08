#!/bin/sh

. ../native/i.sh

for fn in playback w64mpg; do
  compileFile $fn "" "-lasound -lsndfile -lsamplerate" ""
done

for fn in sender; do
  compileFile $fn "" "-lsndfile -lsamplerate" ""
done

for fn in visMeterLoc; do
  compileFile $fn "" "-lasound -lm" ""
done

for fn in receiver recorder streamer streamerBth streamerLft streamerRgt w64play w64rec; do
  compileFile $fn "" "-lasound" ""
done

for fn in visMeterRem; do
  compileFile $fn "" "-lm" ""
done

for fn in forwarder w64fix; do
  compileFile $fn "" "" ""
done
