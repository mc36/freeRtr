#!/bin/sh
for fn in *.tmpl ; do
  java optimizer $fn
  done
echo "##undef _TABLE_SIZE_P4_" > ../p4src/rare_profiles.p4
for fn in *.p4 ; do
  echo "#include \"../profiles/$fn\"" >> ../p4src/rare_profiles.p4
  done