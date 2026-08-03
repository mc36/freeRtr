#!/bin/sh
tail -f -c +0 eth1.pcap | wireshark -k -i -
