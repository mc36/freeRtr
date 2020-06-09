#!/bin/bash

set -e

set -a
. "$1"
set +a

case "$MODE" in
map)
	exec /usr/bin/mapInt.bin "$DEVICE" "$LPORT" "$RADDR" "$RPORT" "$LADDR"
	;;
raw)
	exec /usr/bin/rawInt.bin "$DEVICE" "$LPORT" "$RADDR" "$RPORT" "$LADDR"
	;;
pcap)
	exec /usr/bin/pcapInt.bin "$DEVICE" "$LPORT" "$RADDR" "$RPORT" "$LADDR"
	;;
tap)
	echo '
		sleep 1
		ip -4 addr flush dev "$DEVICE"
		if [ -n "$ADDR4" ]; then
			if [ -n "$PEER4" ]; then
				ip addr add "$ADDR4" peer "$PEER4" dev "$DEVICE"
			else
				ip addr add "$ADDR4" dev "$DEVICE"
			fi
		fi
		if [ -n "$ADDR6" ]; then
			if [ -n "$PEER6" ]; then
				ip addr add "$ADDR6" peer "$PEER6" dev "$DEVICE"
			else
				ip addr add "$ADDR6" dev "$DEVICE"
			fi
		fi
	' | envsubst | bash &
	exec /usr/bin/tapInt.bin "$DEVICE" "$LPORT" "$RADDR" "$RPORT" "$LADDR" 0.0.0.1 255.255.255.255
	;;
std)
	exec /usr/bin/stdLin.bin "$LPORT"
	;;
tty)
	exec /usr/bin/ttyLin.bin "$DEVICE" "$LPORT"
	;;
else)
	echo "Unknown mode $MODE" >&2
	exit 1
esac
