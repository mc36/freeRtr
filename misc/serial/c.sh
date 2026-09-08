#!/bin/sh

. ../native/i.sh

for fn in syncEmu; do
  compileLib $fn "" ""
done

for fn in hdlcInt asyncLin syncInt syncClk stdLin ttyCtr ttyLin modem dummyCon; do
  compileFile $fn "" "-lpthread" ""
done
