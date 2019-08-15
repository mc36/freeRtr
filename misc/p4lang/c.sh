#https://launchpad.net/~frederic-loui/+archive/ubuntu/p4lang-xenial
#sudo apt-get install p4lang-xenial
p4c --std p4-16 --target bmv2 --arch v1model --p4runtime-files vpn-over-bgp-isis-sr-operation.txt vpn-over-bgp-isis-sr-operation.p4 
