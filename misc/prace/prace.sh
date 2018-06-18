#!/bin/bash

# Prace routing aktualizalo a login node-okra.
# Naponta egyszer-ketszer a cronbol futtatjuk.
# 
# v1.0  2017-07-05 kissg@niif.hu
# v1.1  2017-07-05 kissg@niif.hu: a sajat halozatunkra nem kell route-olni
# v1.01 2017-07-06 kissg@niif.hu: a 3-as Bash-hez kicsit rontani kellett rajta
#       2017-08-29 kissg: valtozott a $ROUTE_SERVER

# "Az inputunk Mate Csaba tamogatasaval keszul." :-)
ROUTE_SERVER=x.x.x.x
URL=http://$ROUTE_SERVER/prace-routes.tcl

# Ezzel cimkezzuk meg az itt beszurt route-okat
REALM=prace

PATH=/sbin:/bin:/usr/sbin:/usr/bin
unset LC_ALL
export LANG=C

[[ "$1" = -debug ]] && DEBUG=":"

# Megmondja, hogy az adott IP cim melyik subnetben van
get_network () {
	local OLDIFS="$IFS"
	local IFS='./'
	set -- $1
	IFS="$OLDIFS"
	local prefix=$5
# Magia innen:
# https://stackoverflow.com/questions/10278513/bash-shell-decimal-to-binary-conversion
# es innen:
# https://unix.stackexchange.com/questions/65280/binary-to-hexadecimal-and-decimal-in-a-shell-script
# Picit rontani kellett az elson, hogy a 3-as Bash is egye. ($binlist valtozo)
	local binlist=$(echo {0..1}{0..1}{0..1}{0..1}{0..1}{0..1}{0..1}{0..1})
	declare -a D2B=($binlist)
	local z=${D2B[0]} # nyolc 0
	echo "${D2B[$1]}${D2B[$2]}${D2B[$3]}${D2B[$4]}" | # dec to bin
	sed -r "s/^(.{$prefix}).*/\1$z$z$z$z/;
		s/(.{8})/\1 /g" | {
	 read o1 o2 o3 o4 _rest
	 echo $((2#$o1)).$((2#$o2)).$((2#$o3)).$((2#$o4))/$prefix # bin to dec
	}
}

################### main #####################

if ! fgrep -q $REALM /etc/iproute2/rt_realms
then
	echo "Add '163 $REALM' to file /etc/iproute2/rt_realms. Aborting..." >&2
	exit 1
fi

# Kitalaljuk a gateway-t es az output interfeszt. Utobbi minden gepen mas.
exec 3< <(ip route list exact $ROUTE_SERVER)
read -u 3 webaddr _via_ router _dev_ interface

if [[ -z $interface ]]
then
	echo "Missing route to $ROUTE_SERVER. Aborting..." >&2
	exit 1
fi

# Kitalajuk a sajat networkunket
exec 3< <(ip -4 address show dev $interface | tail -1)
read -u 3 _inet_ myaddr rest
mynetwork=$(get_network $myaddr)

# Kinyerjuk az aktualis PRACE networkoket a route serverbol
declare -a new_routes=(
	$(curl -s $URL |
	grep -v '/3[012]' | # nem igazi subnetek, hanem pont-pont kapcsolatok
	grep -v " $mynetwork " | # a sajat halozatunkra nem kell route-olni
	sed -rn /^B/'s!^B +([0-9./]+).*!\1!p' )
)
#echo ${#new_routes[*]}


# Ezek a statikus route-ok vannak most a kernelben
declare -a old_routes=($(ip route show realm $REALM | cut -f1 -d' ' ))
#echo "${#old_routes[*]}"

# Nehogy valami hiba folytan kitorljunk mindent!
if (( ${#new_routes[*]} < ${#old_routes[*]} / 2 ))
then
	echo "Too few routes from root server. Check manually. Aborting..." >&2
	exit 1
fi


# Lassuk a valtozasokat es aszerint modositsuk a routingot!
# Tulajdonkeppen eleg lenne siman torolni minden regit, es felvenni
# minden ujat, sortolas es diff nelkul, de igy elegansabb a kijelzes
diff --normal	<(echo ${old_routes[*]} | tr ' ' '\012' | sort -n -t. ) \
		<(echo ${new_routes[*]} | tr ' ' '\012' | sort -n -t. ) |
sort |
while read marker subnet
do
	case $marker in
	'<')
		echo Removing subnet $subnet from routing table
		$DEBUG ip route delete $subnet via $router dev $interface realm $REALM
		;;
	'>')
		echo Adding subnet $subnet to routing table
		$DEBUG ip route add $subnet via $router dev $interface realm $REALM
		;;
	esac
done
