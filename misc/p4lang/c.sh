#sudo deb https://download.opensuse.org/repositories/home:/frederic-loui:/p4lang:/p4c:/master/Debian_10/ ./
#wget https://download.opensuse.org/repositories/home:/frederic-loui:/p4lang:/p4c:/master/Debian_10/Release.key
#sudo apt-key add ./Release.key
#sudo apt-get install p4c bmv2
p4c --std p4-16 --target bmv2 --arch v1model --p4runtime-files router.txt router.p4
