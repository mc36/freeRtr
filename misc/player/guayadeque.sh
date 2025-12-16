#!/bin/sh
while ((1)); do
  dbus-send --print-reply --type=method_call --dest=org.mpris.guayadeque /Player org.freedesktop.MediaPlayer.Next
  sleep 3
  done
