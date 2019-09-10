#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import argparse, grpc, os, sys, socket
from time import sleep

# set our lib path
sys.path.append(
    os.path.join(os.path.dirname(os.path.abspath(__file__)),
        './'))
# And then we import
import p4runtime_lib.bmv2
import p4runtime_lib.helper


def writeVrfRules(delete, p4info_helper, ingress_sw, port, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_vrf",
        match_fields={
            "md.source_id": port
        },
        action_name="ctl_ingress.act_set_vrf",
        action_params={
            "vrf": vrf
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeVlanRules(delete, p4info_helper, ingress_sw, port, main, vlan):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_vlan_in",
        match_fields={
            "std_md.ingress_port": main,
            "hdr.vlan.vlan": vlan
        },
        action_name="ctl_ingress.act_set_iface",
        action_params={
            "src": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_vlan_out",
        match_fields={
            "md.target_id": port,
        },
        action_name="ctl_ingress.act_set_vlan_port",
        action_params={
            "port": main,
            "vlan": vlan
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeForwardRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeForwardRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeVpnRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, egress_label, vpn_label):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_mpls_encap_set_nexthop",
        action_params={
            "vpn_label": vpn_label,
            "egress_label": egress_label,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeVpnRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, egress_label, vpn_label):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_mpls_encap_set_nexthop",
        action_params={
            "vpn_label": vpn_label,
            "egress_label": egress_label,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeMyaddrRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_cpl_set_nexthop",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeMyaddrRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_cpl_set_nexthop",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeNexthopRules(delete, p4info_helper, ingress_sw, port, mac_addr):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_nexthop",
        match_fields={
            "md.nexthop_id": port,
        },
        action_name="ctl_ingress.act_ipv4_fib_hit",
        action_params={
            "dst_mac_addr": mac_addr,
            "egress_port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeNeighborRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, port, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv4_fib_host",
        match_fields={
            "hdr.ipv4.dst_addr": dst_ip_addr,
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeNeighborRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, port, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv6_fib_host",
        match_fields={
            "hdr.ipv6.dst_addr": dst_ip_addr,
            "md.l3_metadata.vrf": vrf
        },
        action_name="ctl_ingress.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeMplsRules(delete, p4info_helper, ingress_sw, dst_label, new_label, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_mpls_fib",
        match_fields={
            "md.tunnel_metadata.mpls_label": (dst_label)
        },
        action_name="ctl_ingress.act_mpls_swap_set_nexthop",
        action_params={
            "egress_label": new_label,
            "nexthop_id": port
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_mpls_fib_decap",
        match_fields={
            "md.tunnel_metadata.mpls_label": (dst_label)
        },
        action_name="ctl_ingress.act_mpls_swap_set_nexthop",
        action_params={
            "egress_label": new_label,
            "nexthop_id": port
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)

def writeMyMplsRules(delete, p4info_helper, ingress_sw, dst_label, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_mpls_fib",
        match_fields={
            "md.tunnel_metadata.mpls_label": (dst_label)
        },
        action_name="ctl_ingress.act_mpls_decap_ipv4",
        action_params={
            "vrf": vrf
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_mpls_fib_decap",
        match_fields={
            "md.tunnel_metadata.mpls_label": (dst_label)
        },
        action_name="ctl_ingress.act_mpls_decap_l3vpn",
        action_params={
            "vrf": vrf
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def main(p4info_file_path, bmv2_file_path, p4runtime_address, freerouter_address, freerouter_port):
    p4info_helper = p4runtime_lib.helper.P4InfoHelper(p4info_file_path)

    sck = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sck.connect((freerouter_address, int(freerouter_port)))
    fil = sck.makefile('rw')

    sw1 = p4runtime_lib.bmv2.Bmv2SwitchConnection(
        name='sw1',
        address=p4runtime_address,
        device_id=0,
        proto_dump_file='p4runtime-requests.txt')
    sw1.MasterArbitrationUpdate()
    sw1.SetForwardingPipelineConfig(p4info=p4info_helper.p4info,
                                    bmv2_json_file_path=bmv2_file_path)


    while 1:
        line = fil.readline(8192)
        splt = line.split(" ")
        print "rx: ", splt


        if splt[0] == "route4_add":
            addr = splt[1].split("/");
            writeForwardRules4(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "route4_mod":
            addr = splt[1].split("/");
            writeForwardRules4(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "route4_del":
            addr = splt[1].split("/");
            writeForwardRules4(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue

        if splt[0] == "labroute4_add":
            addr = splt[1].split("/");
            writeForwardRules4(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "labroute4_mod":
            addr = splt[1].split("/");
            writeForwardRules4(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "labroute4_del":
            addr = splt[1].split("/");
            writeForwardRules4(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue

        if splt[0] == "vpnroute4_add":
            addr = splt[1].split("/");
            writeVpnRules4(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "vpnroute4_mod":
            addr = splt[1].split("/");
            writeVpnRules4(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "vpnroute4_del":
            addr = splt[1].split("/");
            writeVpnRules4(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if splt[0] == "myaddr4_add":
            addr = splt[1].split("/");
            writeMyaddrRules4(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[3]))
            continue
        if splt[0] == "myaddr4_mod":
            addr = splt[1].split("/");
            writeMyaddrRules4(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[3]))
            continue
        if splt[0] == "myaddr4_del":
            addr = splt[1].split("/");
            writeMyaddrRules4(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[3]))
            continue

        if splt[0] == "label4_add":
            writeMplsRules(1,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue
        if splt[0] == "label4_mod":
            writeMplsRules(2,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue
        if splt[0] == "label4_del":
            writeMplsRules(3,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue

        if splt[0] == "mylabel4_add":
            writeMyMplsRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "mylabel4_mod":
            writeMyMplsRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "mylabel4_del":
            writeMyMplsRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if splt[0] == "neigh4_add":
            writeNexthopRules(1,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules4(1,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh4_mod":
            writeNexthopRules(2,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules4(2,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh4_del":
            writeNexthopRules(3,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules4(3,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue


        if splt[0] == "portvrf_add":
            writeVrfRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "portvrf_mod":
            writeVrfRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "portvrf_del":
            writeVrfRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if splt[0] == "portvlan_add":
            writeVlanRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "portvrf_mod":
            writeVlanRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "portvrf_del":
            writeVlanRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue


        if splt[0] == "route6_add":
            addr = splt[1].split("/");
            writeForwardRules6(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "route6_mod":
            addr = splt[1].split("/");
            writeForwardRules6(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "route6_del":
            addr = splt[1].split("/");
            writeForwardRules6(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue

        if splt[0] == "labroute6_add":
            addr = splt[1].split("/");
            writeForwardRules6(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "labroute6_mod":
            addr = splt[1].split("/");
            writeForwardRules6(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue
        if splt[0] == "labroute6_del":
            addr = splt[1].split("/");
            writeForwardRules6(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue

        if splt[0] == "vpnroute6_add":
            addr = splt[1].split("/");
            writeVpnRules6(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "vpnroute6_mod":
            addr = splt[1].split("/");
            writeVpnRules6(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "vpnroute6_del":
            addr = splt[1].split("/");
            writeVpnRules6(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if splt[0] == "myaddr6_add":
            addr = splt[1].split("/");
            writeMyaddrRules6(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[3]))
            continue
        if splt[0] == "myaddr6_mod":
            addr = splt[1].split("/");
            writeMyaddrRules6(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[3]))
            continue
        if splt[0] == "myaddr6_del":
            addr = splt[1].split("/");
            writeMyaddrRules6(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[3]))
            continue

        if splt[0] == "label6_add":
            writeMplsRules(1,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue
        if splt[0] == "label6_mod":
            writeMplsRules(2,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue
        if splt[0] == "label6_del":
            writeMplsRules(3,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue

        if splt[0] == "mylabel6_add":
            writeMyMplsRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "mylabel6_mod":
            writeMyMplsRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "mylabel6_del":
            writeMyMplsRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if splt[0] == "neigh6_add":
#            writeNexthopRules(1,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules6(1,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh6_mod":
#            writeNexthopRules(2,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules6(2,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh6_del":
#            writeNexthopRules(3,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules6(3,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue






if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='P4Runtime Controller')

    parser.add_argument('--p4info', help='p4info proto in text format from p4c',
            type=str, action="store", required=False,
            default="../build/router.txt")
    parser.add_argument('--bmv2-json', help='BMv2 JSON file from p4c',
            type=str, action="store", required=False,
            default="../build/router.json")
    parser.add_argument('--p4runtime_address', help='p4 runtime address',
            type=str, action="store", required=False,
            default="127.0.0.1:50051")
    parser.add_argument('--freerouter_address', help='freerouter address',
            type=str, action="store", required=False,
            default="10.10.10.227")
    parser.add_argument('--freerouter_port', help='freerouter port',
            type=str, action="store", required=False,
            default="9080")
    args = parser.parse_args()

    if not os.path.exists(args.p4info):
        parser.print_help()
        print "p4info file not found."
        parser.exit(1)
    if not os.path.exists(args.bmv2_json):
        parser.print_help()
        print "BMv2 JSON file not found."
        parser.exit(1)

    main(args.p4info, args.bmv2_json, args.p4runtime_address, args.freerouter_address, args.freerouter_port)
