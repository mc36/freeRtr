proc doDisPROTOpack {prot pack} {
global packetSimp
global packetVerb
if {$prot == 17} {
  doDisUDPpack $pack
  return
  }
if {$prot == 6} {
  doDisTCPpack $pack
  return
  }
if {$prot == 1} {
  doDisICMP4pack $pack
  return
  }
if {$prot == 58} {
  doDisICMP6pack $pack
  return
  }
if {$prot == 47} {
  doDisGREpack $pack
  return
  }
if {$prot == 115} {
  doDisL2TP3pack $pack
  return
  }
if {$prot == 4} {
  doDisIP4pack $pack
  return
  }
if {$prot == 41} {
  doDisIP6pack $pack
  return
  }
set packetSimp "$packetSimp proto=$prot"
return
}
