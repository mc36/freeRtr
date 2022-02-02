#!/bin/sh
for fn in *.tmpl ; do
  java optimizer $fn
  done
echo "##undef _TABLE_SIZE_P4_" > ../p4src/rare_profiles.p4
cat *.p4 >> ../p4src/rare_profiles.p4
