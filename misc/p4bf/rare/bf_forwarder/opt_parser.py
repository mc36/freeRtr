from ..bf_gbl_env.var_env import *

def get_opt_parser():
    parser = argparse.ArgumentParser(description="BfRuntime controller")

    parser.add_argument(
        "--bfruntime-address",
        help="BfRuntime address",
        type=str,
        action="store",
        required=False,
        default="127.0.0.1:50052",
    )
    parser.add_argument(
        "--freerouter-address",
        help="freerouter address",
        type=str,
        action="store",
        required=False,
        default="127.0.0.1",
    )
    parser.add_argument(
        "--freerouter-port",
        help="freerouter port",
        type=int,
        action="store",
        required=False,
        default=9080,
    )
    parser.add_argument(
        "--p4-program-name",
        help="run bf_forwarder.py agaisnt p4 program",
        type=str,
        action="store",
        required=False,
        default="bf_router",
    )
    parser.add_argument(
        "--client-id",
        help="bf_forwarder.py gprc client-id",
        type=int,
        action="store",
        required=False,
        default=0,
    )
    parser.add_argument(
        "--pipe-name",
        help="bf_forwarder.py grpc pipe-name",
        type=str,
        action="store",
        required=False,
        default="pipe",
    )
    parser.add_argument(
        "--brdg",
        help="enable bridge",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--mpls",
        help="enable mpls",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--srv6",
        help="enable srv6",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--polka",
        help="enable polka",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--nsh",
        help="enable nsh",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--nat",
        help="enable nat",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--pbr",
        help="enable pbr",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--tun",
        help="enable tunnel",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--poe",
        help="enable pppoe",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--mcast",
        help="enable multicast",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--snmp",
        help="enable snmp export locally",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=False,
    )
    parser.add_argument(
        "--ifmibs-dir",
        help="Path to the directory where the interface MIBs are stored",
        type=str,
        action="store",
        required=False,
        default="/var/run/bf_router",
    )
    parser.add_argument(
        "--stats-interval",
        help="Interval in seconds between updates of the MIB objects",
        type=int,
        action="store",
        required=False,
        default=5,
    )
    parser.add_argument(
        "--ifindex",
        help="Path to the ifindex MIB file definition",
        type=str,
        action="store",
        required=False,
        default="/root/rare/snmp/ifindex",
    )
    parser.add_argument(
        "--platform",
        help="Platform used: accton_wedge100bf_32x, accton_wedge100bf_32qs, accton_wedge100bf_65x, stordis_bf2556x_1t, stordis_bf6064x_t",
        type=str,
        action="store",
        required=False,
        default="accton_wedge100bf_32x",
    )
    parser.add_argument(
        "--sal-grpc-server-address",
        help="SAL GRPC server address",
        type=str,
        action="store",
        required=False,
        default="127.0.0.1:50054",
    )
    parser.add_argument(
        "--no-log-keepalive",
        help="Whether to suppress logging of keepalive messages",
        action="store_true",
        required=False,
        default=False
    )
    args = parser.parse_args()
    
    return(args)
