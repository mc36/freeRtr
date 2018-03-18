#!/bin/sh
export TOOLPATH=/usr/lib/android-sdk/build-tools/debian
$TOOLPATH/dx --dex --output=rtr.dex ../../src/rtr.jar
$TOOLPATH/aapt package -f -M AndroidManifest.xml -I /usr/lib/android-sdk/platforms/android-*/android.jar -F rtr.apk
$TOOLPATH/aapt add -f rtr.apk rtr.dex
