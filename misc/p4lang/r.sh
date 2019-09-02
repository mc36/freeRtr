sudo ifconfig ens3 up
sudo ifconfig ens4 up
sudo ifconfig ens5 up
sudo simple_switch_grpc -i 1@ens3 -i 2@ens4 -i 64@ens5 --thrift-port 9090 vpn-over-bgp-isis-sr-operation-core1-ler.json
