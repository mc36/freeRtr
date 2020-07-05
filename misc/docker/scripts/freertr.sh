#!/bin/sh

iflag=false
rflag=false
FREERTR_INTF_LIST=""
FREERTR_HOSTNAME=""
FREERTR_BASE_DIR=$(pwd)

usage(){
        echo "Usage: `basename $0` -i <intf/port1/port2> -r <freertr-hostname> -h for help";
        echo "Example: $0 -i \"eth0/22705/22706 eth1/20010/20011\" -r freertr"
        exit 1
}

bindintf () {
    echo "bindintf: FREERTR_INTF_LIST=$FREERTR_INTF_LIST";
    FREERTR_INTF_LIST=$(echo $1 | tr -d '\"');

    echo "--- DECLARING FREERTR INTERFACE RAWINT (FORWARDING PLANE) ---";
    for FREERTR_INTF in $FREERTR_INTF_LIST;
      do
        echo "FREERTR_INTF=$FREERTR_INTF";
        IFS=/;
        set $FREERTR_INTF;
        echo "INTF=$1";
        echo "PORT_1=$2";
        echo "PORT_2=$3";
        ip link set $1 up multicast on promisc on mtu 1500
        ethtool -K $1 rx off
        ethtool -K $1 tx off
        ethtool -K $1 sg off
        ethtool -K $1 tso off
        ethtool -K $1 ufo off
        ethtool -K $1 gso off
        ethtool -K $1 gro off
        ethtool -K $1 lro off
        ethtool -K $1 rxvlan off
        ethtool -K $1 txvlan off
        ethtool -K $1 ntuple off
        ethtool -K $1 rxhash off
        ethtool --set-eee $1 eee off

        start-stop-daemon -S -b -x "${FREERTR_BASE_DIR}/bin/rawInt.bin" $1 $3 127.0.0.1 $2 127.0.0.1;
      done
}

start_freertr () {
  FREERTR_BASE_DIR=$1
  FREERTR_HOSTNAME=$2
  cd "${FREERTR_BASE_DIR}/run"
  java -jar "${FREERTR_BASE_DIR}/bin/rtr.jar" routercs "${FREERTR_BASE_DIR}/run/${FREERTR_HOSTNAME}-hw.txt" "${FREERTR_BASE_DIR}/run/${FREERTR_HOSTNAME}-sw.txt"
}



if ( ! getopts ":hi:r:" opt); then
        usage
        exit $E_OPTERROR;
fi

while getopts ":hi:r:" opt;do
case $opt in
  i)
    FREERTR_INTF_LIST=$OPTARG
    echo "FREERTR_INTF_LIST: $FREERTR_INTF_LIST";
    iflag=true
  ;;
  r)
    FREERTR_HOSTNAME=$OPTARG
    echo "FREERTR_HOSTNAME: $FREERTR_HOSTNAME";
    rflag=true
  ;;
  \?)
     echo "Option not supported." >&2
     usage
     exit 1
  ;;
  :)
    echo "Option -$OPTARG requires an argument." >&2
    usage
    exit 1
  ;;
  h|*)
   usage
   exit 1
  ;;
  esac
done

if $iflag && $rflag ;
then
   bindintf "${FREERTR_INTF_LIST}" "${FREERTR_BASE_DIR}"
   start_freertr "${FREERTR_BASE_DIR}" ${FREERTR_HOSTNAME}
else
   if ! $iflag; then echo "[-i] freertr interface list missing"
   usage
   fi
   if ! $rflag; then echo "[-r] router hostname missing"
   usage
   fi
fi
