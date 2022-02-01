#!/bin/sh
javac -source 11 -target 11 -Xlint:all -deprecation *.java
java optimizer profile-bng
java optimizer profile-bras
java optimizer profile-cgnat
java optimizer profile-cleaner
java optimizer profile-cpe
java optimizer profile-geant-testbed
java optimizer profile-gre
java optimizer profile-ipip
java optimizer profile-nfv
java optimizer profile-nop-mchome
java optimizer profile-p
java optimizer profile-pe
java optimizer profile-rawip
java optimizer profile-renater-peering-l2
java optimizer profile-renater-peering-l3
java optimizer profile-srv6
java optimizer profile-tor
java optimizer profile-wlc
rm *.class 2> /dev/null
echo "##undef _TABLE_SIZE_P4_" > ../p4src/rare_profiles.p4
cat *.p4 >> ../p4src/rare_profiles.p4
