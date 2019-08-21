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



def writeForwardRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask)
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

def writeNexthopRules(delete, p4info_helper, ingress_sw, port, mac_addr):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_nexthop",
        match_fields={
            "md.nexthop_id": port,
            "md.intrinsic_metadata.priority": 0
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

def writeNeighborRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, port):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_ipv4_fib_host",
        match_fields={
            "hdr.ipv4.dst_addr": dst_ip_addr
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

def writeMplsRules4(delete, p4info_helper, ingress_sw, dst_label, new_label, port):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ctl_ingress.tbl_mpls_fib",
        match_fields={
            "hdr.mpls[0].label": (dst_label)
        },
        action_name="ctl_ingress.act_mpls_swap_set_nexthop",
        action_params={
            "egress_label": new_label,
            "nexthop_id": port
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

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
            writeForwardRules4(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]))
            continue
        if splt[0] == "route4_mod":
            addr = splt[1].split("/");
            writeForwardRules4(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]))
            continue
        if splt[0] == "route4_del":
            addr = splt[1].split("/");
            writeForwardRules4(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]))
            continue
        if splt[0] == "label4_add":
            writeMplsRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue
        if splt[0] == "label4_mod":
            writeMplsRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue
        if splt[0] == "label4_del":
            writeMplsRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue
        if splt[0] == "neigh4_add":
            writeNexthopRules(1,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules4(1,p4info_helper,sw1,splt[2],int(splt[1]))
            continue
        if splt[0] == "neigh4_mod":
            writeNexthopRules(2,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules4(2,p4info_helper,sw1,splt[2],int(splt[1]))
            continue
        if splt[0] == "neigh4_del":
            writeNexthopRules(3,p4info_helper,sw1,int(splt[1]),splt[3])
            writeNeighborRules4(3,p4info_helper,sw1,splt[2],int(splt[1]))
            continue


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='P4Runtime Controller')

    parser.add_argument('--p4info', help='p4info proto in text format from p4c',
            type=str, action="store", required=False,
            default="./vpn-over-bgp-isis-sr-operation.txt")
    parser.add_argument('--bmv2-json', help='BMv2 JSON file from p4c',
            type=str, action="store", required=False,
            default="./vpn-over-bgp-isis-sr-operation.json")
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
