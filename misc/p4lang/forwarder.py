#!/usr/bin/env python3
# -*- coding: utf-8 -*-

###############################################################################
#
# Copyright 2019-present GEANT RARE project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed On an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
###############################################################################

import argparse, grpc, os, sys, socket
from time import sleep

# set our lib path
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)),'./','/usr/lib/python3.9/site-packages/'))
# And then we import
import p4runtime_lib.bmv2
import p4runtime_lib.helper


mcast = []


def writeVrfRules(delete, p4info_helper, ingress_sw, port, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vrf.tbl_vrf",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_vrf.act_set_vrf",
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
        table_name="ig_ctl.ig_ctl_vlan_in.tbl_vlan_in",
        match_fields={
            "ig_md.ingress_id": main,
            "hdr.vlan.vid": vlan
        },
        action_name="ig_ctl.ig_ctl_vlan_in.act_set_iface",
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
        table_name="eg_ctl.eg_ctl_vlan_out.tbl_vlan_out",
        match_fields={
            "eg_md.target_id": port,
        },
        action_name="eg_ctl.eg_ctl_vlan_out.act_set_vlan_port",
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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_outport.tbl_vlan_out",
        match_fields={
            "ig_md.target_id": port,
        },
        action_name="ig_ctl.ig_ctl_outport.act_set_port",
        action_params={
            "port": main,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeNhop2portRules(delete, p4info_helper, ingress_sw, nhop, subif, port):
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_outport.tbl_nexthop",
        match_fields={
            "ig_md.nexthop_id": nhop,
        },
        action_name="ig_ctl.ig_ctl_outport.act_set_port",
        action_params={
            "port": port,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeBunVlanRules(delete, p4info_helper, ingress_sw, main, vlan, port):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vlan_in.tbl_vlan_in",
        match_fields={
            "ig_md.ingress_id": main,
            "hdr.vlan.vid": vlan
        },
        action_name="ig_ctl.ig_ctl_vlan_in.act_set_iface",
        action_params={
            "src": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeBundleRules(delete, p4info_helper, ingress_sw, port, hsh, trg):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bundle.tbl_bundle",
        match_fields={
            "ig_md.outport_id": port,
            "ig_md.hash_id": hsh
        },
        action_name="ig_ctl.ig_ctl_bundle.act_set_hash",
        action_params={
            "port": trg
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeHairpinRules(delete, p4info_helper, ingress_sw, port, trg):
    for hsh in range(0, 16):
        table_entry1 = p4info_helper.buildTableEntry(
            table_name="ig_ctl.ig_ctl_bundle.tbl_bundle",
            match_fields={
                "ig_md.outport_id": port,
                "ig_md.hash_id": hsh
            },
            action_name="ig_ctl.ig_ctl_bundle.act_set_recir",
            action_params={
                "port": trg
            })
        if delete == 1:
            ingress_sw.WriteTableEntry(table_entry1, False)
        elif delete == 2:
            ingress_sw.ModifyTableEntry(table_entry1, False)
        else:
            ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_hairpin.tbl_hairpin",
        match_fields={
            "eg_md.outport_id": port,
        },
        action_name="eg_ctl.eg_ctl_hairpin.act_set_recir",
        action_params={
            "port": trg
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeGre4rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 47,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_gre",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_gre4",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeGre6rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 47,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_gre",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_gre6",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)



def writeIpip4rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 4,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_ip4ip",
        action_params={
            "port": port
        })
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 41,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_ip6ip",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_ipip4",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeIpip6rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 4,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_ip4ip",
        action_params={
            "port": port
        })
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 41,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_ip6ip",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_ipip6",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)



def writeL2tp4rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt, tid):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 17,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": dprt,
            "ig_md.layer4_dstprt": sprt,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_l2tp",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_l2tp4",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
            "src_port": sprt,
            "dst_port": dprt,
            "tunnel_id": tid,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeL2tp6rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt, tid):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 17,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": dprt,
            "ig_md.layer4_dstprt": sprt,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_l2tp",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_l2tp6",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
            "src_port": sprt,
            "dst_port": dprt,
            "tunnel_id": tid,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeVxlan4rules(delete, p4info_helper, ingress_sw, bridge, addr, sip, dip, nexthop, instance, vrf, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_vxlan4",
        action_params={
            "nexthop": nexthop,
            "dst_ip_addr": dip,
            "src_ip_addr": sip,
            "instance": instance
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 17,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": 4789,
            "ig_md.layer4_dstprt": 4789,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_vxlan",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeVxlan6rules(delete, p4info_helper, ingress_sw, bridge, addr, sip, dip, nexthop, instance, vrf, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_vxlan6",
        action_params={
            "nexthop": nexthop,
            "dst_ip_addr": dip,
            "src_ip_addr": sip,
            "instance": instance
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 17,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": 4789,
            "ig_md.layer4_dstprt": 4789,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_vxlan",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writePckoudp4rules(delete, p4info_helper, ingress_sw, bridge, addr, sip, dip, sprt, dprt, nexthop, vrf, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_pckoudp4",
        action_params={
            "nexthop": nexthop,
            "dst_ip_addr": dip,
            "src_ip_addr": sip,
            "src_port": sprt,
            "dst_port": dprt,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 17,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": dprt,
            "ig_md.layer4_dstprt": sprt,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_pckoudp",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writePckoudp6rules(delete, p4info_helper, ingress_sw, bridge, addr, sip, dip, sprt, dprt, nexthop, vrf, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_pckoudp6",
        action_params={
            "nexthop": nexthop,
            "dst_ip_addr": dip,
            "src_ip_addr": sip,
            "src_port": sprt,
            "dst_port": dprt,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 17,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": dprt,
            "ig_md.layer4_dstprt": sprt,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_pckoudp",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writePppoeRules(delete, p4info_helper, ingress_sw, port, phport, nexthop, vrf, ses, dmac, smac):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pppoe.tbl_pppoe",
        match_fields={
            "ig_md.source_id": phport,
            "hdr.pppoeD.session": ses
        },
        action_name="ig_ctl.ig_ctl_pppoe.act_pppoe_data",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_pppoe",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "session": ses
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeXconnRules(delete, p4info_helper, ingress_sw, port, target, lab_tun, lab_loc, lab_rem):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": lab_loc
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_l2vpn",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": lab_loc
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_l2vpn",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vrf.tbl_vrf",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_vrf.act_set_mpls_xconn_encap",
        action_params={
            "target": target,
            "tunlab": lab_tun,
            "svclab": lab_rem
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeBrprtRules(delete, p4info_helper, ingress_sw, port, bridge):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vrf.tbl_vrf",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_vrf.act_set_bridge",
        action_params={
            "bridge": bridge
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeBrlabRules(delete, p4info_helper, ingress_sw, bridge, label):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": label
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_vpls",
        action_params={
            "bridge": bridge
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": label
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_vpls",
        action_params={
            "bridge": bridge
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeBrsrvRules(delete, p4info_helper, ingress_sw, glob, dst_addr, bridge):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_host",
        match_fields={
            "ig_md.vrf": (glob),
            "hdr.ipv6.dst_addr": (dst_addr)
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_srv_decap_evpn",
        action_params={
            "bridge": bridge
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeBrvplsRules(delete, p4info_helper, ingress_sw, bridge, addr, port, labtun, labsvc):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_vpls",
        action_params={
            "port": port,
            "lab_tun": labtun,
            "lab_svc": labsvc
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeBrsrv6rules(delete, p4info_helper, ingress_sw, bridge, addr, port, target):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_srv",
        action_params={
            "port": port,
            "target": target
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeRoumacRules(delete, p4info_helper, ingress_sw, bridge, addr, nexthop, ppp):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    if ppp == 0:
        actnam = "ig_ctl.ig_ctl_bridge.act_set_bridge_routed"
    else:
        actnam = "ig_ctl.ig_ctl_bridge.act_set_bridge_ppprouted"
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name=actnam,
        action_params={
            "nexthop": nexthop
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)



def writeBrmacRules(delete, p4info_helper, ingress_sw, bridge, addr, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_learn",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.src_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_port",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_bridge.tbl_bridge_target",
        match_fields={
            "ig_md.bridge_id": bridge,
            "hdr.ethernet.dst_mac_addr": addr
        },
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_out",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeForwardRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4b.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4b.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeForwardRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_ipv6_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6b.act_ipv6_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeGlobRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, egress_label):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4.act_ipv4_mpls1_encap_set_nexthop",
        action_params={
            "egress_label": egress_label,
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4b.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4b.act_ipv4_mpls1_encap_set_nexthop",
        action_params={
            "egress_label": egress_label,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeGlobRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, egress_label):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6b.act_ipv6_mpls1_encap_set_nexthop",
        action_params={
            "egress_label": egress_label,
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_ipv6_mpls1_encap_set_nexthop",
        action_params={
            "egress_label": egress_label,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeVpnRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, egress_label, vpn_label):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4.act_ipv4_mpls2_encap_set_nexthop",
        action_params={
            "vpn_label": vpn_label,
            "egress_label": egress_label,
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4b.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4b.act_ipv4_mpls2_encap_set_nexthop",
        action_params={
            "vpn_label": vpn_label,
            "egress_label": egress_label,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeVpnRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, egress_label, vpn_label):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6b.act_ipv6_mpls2_encap_set_nexthop",
        action_params={
            "vpn_label": vpn_label,
            "egress_label": egress_label,
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_ipv6_mpls2_encap_set_nexthop",
        action_params={
            "vpn_label": vpn_label,
            "egress_label": egress_label,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeSrvRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, target):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4.act_ipv4_srv_encap_set_nexthop",
        action_params={
            "target": target,
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4b.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4b.act_ipv4_srv_encap_set_nexthop",
        action_params={
            "target": target,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeSrvRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf, target):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_ipv6_srv_encap_set_nexthop",
        action_params={
            "target": target,
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6b.act_ipv6_srv_encap_set_nexthop",
        action_params={
            "target": target,
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def add2dictIfNot(dic, key, val, msk, cnd):
    if msk == cnd:
        return;
    dic[key] = (val,msk)


def writeCoppRules4(delete, p4info_helper, ingress_sw, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_copp.tbl_ipv4_copp",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_copp.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeCoppRules6(delete, p4info_helper, ingress_sw, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_copp.tbl_ipv6_copp",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_copp.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeInAclRules4(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_in.tbl_ipv4_acl",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_acl_in.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeInAclRules6(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_in.tbl_ipv6_acl",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_acl_in.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeOutAclRules4(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"eg_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "eg_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "eg_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_acl_out.tbl_ipv4_acl",
        match_fields=matches,
        action_name="eg_ctl.eg_ctl_acl_out.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeOutAclRules6(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"eg_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "eg_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "eg_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_acl_out.tbl_ipv6_acl",
        match_fields=matches,
        action_name="eg_ctl.eg_ctl_acl_out.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeNatCfgRules4(delete, p4info_helper, ingress_sw, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nat.tbl_ipv4_nat_cfg",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_nat.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeNatCfgRules6(delete, p4info_helper, ingress_sw, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nat.tbl_ipv6_nat_cfg",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_nat.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeNatTrnsRules4(delete, p4info_helper, ingress_sw, vrf, proto, osa, osp, ota, otp, nsa, nsp, nta, ntp):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nat.tbl_ipv4_nat_trns",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": proto,
            "hdr.ipv4.src_addr": osa,
            "hdr.ipv4.dst_addr": ota,
            "ig_md.layer4_srcprt": osp,
            "ig_md.layer4_dstprt": otp
        },
        action_name="ig_ctl.ig_ctl_nat.act_rewrite_ipv4prt"+str(proto),
        action_params={
            "srcadr": nsa,
            "trgadr": nta,
            "srcprt": nsp,
            "trgprt": ntp,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeNatTrnsRules6(delete, p4info_helper, ingress_sw, vrf, proto, osa, osp, ota, otp, nsa, nsp, nta, ntp):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nat.tbl_ipv6_nat_trns",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": proto,
            "hdr.ipv6.src_addr": osa,
            "hdr.ipv6.dst_addr": ota,
            "ig_md.layer4_srcprt": osp,
            "ig_md.layer4_dstprt": otp
        },
        action_name="ig_ctl.ig_ctl_nat.act_rewrite_ipv6prt"+str(proto),
        action_params={
            "srcadr": nsa,
            "trgadr": nta,
            "srcprt": nsp,
            "trgprt": ntp,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrNormRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv4_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_normal",
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrNormRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv6_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_normal",
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrVrfRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv4_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_setvrf",
        priority=65535-pri,
        action_params={
            "vrf_id": tvrf,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrVrfRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv6_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_setvrf",
        priority=65535-pri,
        action_params={
            "vrf_id": tvrf,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrHopRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv4_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_sethop",
        priority=65535-pri,
        action_params={
            "vrf_id": tvrf,
            "nexthop_id": thop,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrHopRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv6_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_sethop",
        priority=65535-pri,
        action_params={
            "vrf_id": tvrf,
            "nexthop_id": thop,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrLabRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, tlab, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv4_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_setlabel",
        priority=65535-pri,
        action_params={
            "vrf_id": tvrf,
            "nexthop_id": thop,
            "label_val": tlab,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writePbrLabRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, tlab, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_pbr.tbl_ipv6_pbr",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_pbr.act_setlabel",
        priority=65535-pri,
        action_params={
            "vrf_id": tvrf,
            "nexthop_id": thop,
            "label_val": tlab,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeInQosRules4(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_qos_in.tbl_ipv4_qos",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_qos_in.act_"+act,
        priority=65535-pri,
        action_params={
            "metid": (meter+1),
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeInQosRules6(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_qos_in.tbl_ipv6_qos",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_qos_in.act_"+act,
        priority=65535-pri,
        action_params={
            "metid": (meter+1),
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeOutQosRules4(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"eg_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "eg_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "eg_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_qos_out.tbl_ipv4_qos",
        match_fields=matches,
        action_name="eg_ctl.eg_ctl_qos_out.act_"+act,
        priority=65535-pri,
        action_params={
            "metid": (meter+1),
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeOutQosRules6(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"eg_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "eg_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "eg_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_qos_out.tbl_ipv6_qos",
        match_fields=matches,
        action_name="eg_ctl.eg_ctl_qos_out.act_"+act,
        priority=65535-pri,
        action_params={
            "metid": (meter+1),
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeInQosRules(delete, p4info_helper, ingress_sw, meter, bytes, interval):
    metid = p4info_helper.get_meters_id("ig_ctl.ig_ctl_qos_in.policer")
    if delete != 3:
        ingress_sw.WriteMeter(metid, (meter+1), bytes, bytes)

def writeOutQosRules(delete, p4info_helper, ingress_sw, meter, bytes, interval):
    metid = p4info_helper.get_meters_id("eg_ctl.eg_ctl_qos_out.policer")
    if delete != 3:
        ingress_sw.WriteMeter(metid, (meter+1), bytes, bytes)




def writeFlowspecRules4(delete, p4info_helper, ingress_sw, vrf, meter, bytes, interval, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_flowspec.tbl_ipv4_flowspec",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_flowspec.act4_"+act,
        priority=65535-pri,
        action_params={
            "metid": (meter+1),
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)
    metid = p4info_helper.get_meters_id("ig_ctl.ig_ctl_flowspec.policer4")
    if delete != 3:
        ingress_sw.WriteMeter(metid, (meter+1), bytes, bytes)


def writeFlowspecRules6(delete, p4info_helper, ingress_sw, vrf, meter, bytes, interval, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_flowspec.tbl_ipv6_flowspec",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_flowspec.act6_"+act,
        priority=65535-pri,
        action_params={
            "metid": (meter+1),
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)
    metid = p4info_helper.get_meters_id("ig_ctl.ig_ctl_flowspec.policer6")
    if delete != 3:
        ingress_sw.WriteMeter(metid, (meter+1), bytes, bytes)



def writeMyaddrRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4.act_ipv4_cpl_set_nexthop",
        action_params={
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4b.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4b.act_ipv4_cpl_set_nexthop",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeMyaddrRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_ipv6_cpl_set_nexthop",
        action_params={
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6b.act_ipv6_cpl_set_nexthop",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeNexthopRules(delete, p4info_helper, ingress_sw, nexthop, dst_mac_addr, src_mac_addr, port):
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_fib_hit",
        action_params={
            "dst_mac_addr": dst_mac_addr,
            "src_mac_addr": src_mac_addr,
            "egress_port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeNeighborRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, port, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4.tbl_ipv4_fib_host",
        match_fields={
            "hdr.ipv4.dst_addr": dst_ip_addr,
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4b.tbl_ipv4_fib_host",
        match_fields={
            "hdr.ipv4b.dst_addr": dst_ip_addr,
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4b.act_ipv4_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeNeighborRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, port, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_host",
        match_fields={
            "hdr.ipv6.dst_addr": dst_ip_addr,
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_ipv6_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_host",
        match_fields={
            "hdr.ipv6b.dst_addr": dst_ip_addr,
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6b.act_ipv6_set_nexthop",
        action_params={
            "nexthop_id": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeMplsRules(delete, p4info_helper, ingress_sw, dst_label, new_label, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_swap0_set_nexthop",
        action_params={
            "egress_label": new_label,
            "nexthop_id": port
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_swap1_set_nexthop",
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


def writeUnMplsRules(delete, p4info_helper, ingress_sw, dst_label, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_set_nexthop",
        action_params={
            "nexthop_id": port
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_set_nexthop",
        action_params={
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
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_ipv4",
        action_params={
            "vrf": vrf
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_l3vpn",
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


def writeCpuMplsRules(delete, p4info_helper, ingress_sw, dst_label):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_cpulabel",
        action_params={
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_cpulabel",
        action_params={
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


def writeNshFwdRules(delete, p4info_helper, ingress_sw, sp, si, prt, src, dst, tsp, tsi):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nsh.tbl_nsh",
        match_fields={
            "hdr.nsh.sp": (sp),
            "hdr.nsh.si": (si)
        },
        action_name="ig_ctl.ig_ctl_nsh.act_forward",
        action_params={
            "port": prt,
            "src": src,
            "dst": dst,
            "sp": tsp,
            "si": tsi
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeNshLocRules(delete, p4info_helper, ingress_sw, sp, si, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nsh.tbl_nsh",
        match_fields={
            "hdr.nsh.sp": (sp),
            "hdr.nsh.si": (si)
        },
        action_name="ig_ctl.ig_ctl_nsh.act_route",
        action_params={
            "vrf": vrf
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeMySrv4rules(delete, p4info_helper, ingress_sw, glob, dst_addr, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_host",
        match_fields={
            "ig_md.vrf": (glob),
            "hdr.ipv6.dst_addr": (dst_addr)
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_srv_decap_ipv4",
        action_params={
            "vrf": vrf
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeMySrv6rules(delete, p4info_helper, ingress_sw, glob, dst_addr, vrf):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_host",
        match_fields={
            "ig_md.vrf": (glob),
            "hdr.ipv6.dst_addr": (dst_addr)
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_srv_decap_ipv6",
        action_params={
            "vrf": vrf
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeMlocal4rules(delete, p4info_helper, ingress_sw, vrf, sess, dip, sip, ingr, delete2):
    global mcast
    sess = sess & 0xfff
    if delete == 1:
        act = "act_local"
    else:
        act = "act_flood"
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mcast.tbl_mcast4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.src_addr": sip,
            "hdr.ipv4.dst_addr": dip,
        },
        action_name="ig_ctl.ig_ctl_mcast."+act,
        action_params={
            "ingr": ingr,
            "sess": sess
        })
    table_entry2 = p4info_helper.buildMulticastGroupEntry(sess, mcast)
    if delete2 == "add":
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WritePREEntry(table_entry2, False)
    elif delete2 == "mod":
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyPREEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeletePREEntry(table_entry2, False)
    mcast = []


def writeMlocal6rules(delete, p4info_helper, ingress_sw, vrf, sess, dip, sip, ingr, delete2):
    global mcast
    sess = sess & 0xfff
    if delete == 1:
        act = "act_local"
    else:
        act = "act_flood"
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mcast.tbl_mcast6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.src_addr": sip,
            "hdr.ipv6.dst_addr": dip,
        },
        action_name="ig_ctl.ig_ctl_mcast."+act,
        action_params={
            "ingr": ingr,
            "sess": sess
        })
    table_entry2 = p4info_helper.buildMulticastGroupEntry(sess, mcast)
    if delete2 == "add":
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WritePREEntry(table_entry2, False)
    elif delete2 == "mod":
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyPREEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeletePREEntry(table_entry2, False)
    mcast = []


def writeMroute4rules(delete, p4info_helper, ingress_sw, vrf, sess, dip, sip, ingr, port, subif, smac, dmac):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":subif})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": subif
        },
        action_name="eg_ctl.eg_ctl_mcast.act_rawip",
        action_params={
            "src_mac_addr": smac,
            "dst_mac_addr": dmac
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeMroute6rules(delete, p4info_helper, ingress_sw, vrf, sess, dip, sip, ingr, port, subif, smac, dmac):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":subif})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": subif
        },
        action_name="eg_ctl.eg_ctl_mcast.act_rawip",
        action_params={
            "src_mac_addr": smac,
            "dst_mac_addr": dmac
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)




def writeDupLabelRules(delete, p4info_helper, ingress_sw, vrf, sess, inlab, port, subif, hopid, outlab):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":hopid})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": hopid
        },
        action_name="eg_ctl.eg_ctl_mcast.act_duplab",
        action_params={
            "hop": hopid,
            "label": outlab
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeMlabRouteRules(delete, p4info_helper, ingress_sw, ipver, vrf, sess, dip, sip, ingr, port, hopid, outlab, subif):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":hopid})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": hopid
        },
        action_name="eg_ctl.eg_ctl_mcast.act_encap_ipv"+ipver+"_mpls",
        action_params={
            "hop": hopid,
            "label": outlab
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeDupLabLocRules(delete, p4info_helper, ingress_sw, ipver, vrf, sess, inlab, delete2):
    global mcast
    sess = sess & 0xfff
    if delete == 1:
        act = "act_decap_mpls_ipv"+ipver
        mcast.append({"egress_port":0, "instance":0})
    else:
        act = "act_drop"
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": inlab,
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_bcast_label",
        action_params={
            "sess": sess
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": 0
        },
        action_name="eg_ctl.eg_ctl_mcast."+act,
        action_params={}
    )
    table_entry3 = p4info_helper.buildMulticastGroupEntry(sess, mcast)
    if delete2 == "add":
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WritePREEntry(table_entry3, False)
    elif delete2 == "mod":
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyPREEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeletePREEntry(table_entry3, False)
    mcast = []





def writeBierLabelRules(delete, p4info_helper, ingress_sw, vrf, sess, inlab, port, subif, hopid, outlab, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":hopid})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": hopid
        },
        action_name="eg_ctl.eg_ctl_mcast.act_bier",
        action_params={
            "hop": hopid,
            "label": outlab,
            "bs0": bs0,
            "bs1": bs1,
            "bs2": bs2,
            "bs3": bs3,
            "bs4": bs4,
            "bs5": bs5,
            "bs6": bs6,
            "bs7": bs7
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)

def writeBierLabLocRules(delete, p4info_helper, ingress_sw, ipver, vrf, sess, inlab, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7):
    global mcast
    sess = sess & 0xfff
    mcast.append({"egress_port":0, "instance":0})
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": inlab,
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_bier_label",
        action_params={
            "sess": sess
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": 0
        },
        action_name="eg_ctl.eg_ctl_mcast.act_decap_bier_ipv"+ipver,
        action_params={
            "bs0": bs0,
            "bs1": bs1,
            "bs2": bs2,
            "bs3": bs3,
            "bs4": bs4,
            "bs5": bs5,
            "bs6": bs6,
            "bs7": bs7
        }
    )
    table_entry3 = p4info_helper.buildMulticastGroupEntry(sess, mcast)
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WritePREEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyPREEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeletePREEntry(table_entry3, False)
    mcast = []

def writeMbierRouteRules(delete, p4info_helper, ingress_sw, ipver, vrf, sess, dip, sip, ingr, port, hopid, outlab, subif, bfir, si, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":(hopid+si)})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": (hopid+si)
        },
        action_name="eg_ctl.eg_ctl_mcast.act_encap_ipv"+ipver+"_bier",
        action_params={
            "hop": hopid,
            "label": outlab,
            "bfir": bfir, 
            "bs0": bs0,
            "bs1": bs1,
            "bs2": bs2,
            "bs3": bs3,
            "bs4": bs4,
            "bs5": bs5,
            "bs6": bs6,
            "bs7": bs7
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
        print("rx: ", splt)


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
            writeGlobRules4(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue
        if splt[0] == "labroute4_mod":
            addr = splt[1].split("/");
            writeGlobRules4(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue
        if splt[0] == "labroute4_del":
            addr = splt[1].split("/");
            writeGlobRules4(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue

        if splt[0] == "srvroute4_add":
            addr = splt[1].split("/");
            writeSrvRules4(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
            continue
        if splt[0] == "srvroute4_mod":
            addr = splt[1].split("/");
            writeSrvRules4(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
            continue
        if splt[0] == "srvroute4_del":
            addr = splt[1].split("/");
            writeSrvRules4(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
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

        if splt[0] == "copp4_add":
            writeCoppRules4(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]),splt[5],splt[6],splt[7],splt[8],int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]))
            continue
        if splt[0] == "copp4_mod":
            writeCoppRules4(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]),splt[5],splt[6],splt[7],splt[8],int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]))
            continue
        if splt[0] == "copp4_del":
            writeCoppRules4(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]),splt[5],splt[6],splt[7],splt[8],int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]))
            continue

        if splt[0] == "copp6_add":
            writeCoppRules6(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]),splt[5],splt[6],splt[7],splt[8],int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]))
            continue
        if splt[0] == "copp6_mod":
            writeCoppRules6(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]),splt[5],splt[6],splt[7],splt[8],int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]))
            continue
        if splt[0] == "copp6_del":
            writeCoppRules6(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]),splt[5],splt[6],splt[7],splt[8],int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]))
            continue

        if splt[0] == "inacl4_add":
            writeInAclRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "inacl4_mod":
            writeInAclRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "inacl4_del":
            writeInAclRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue

        if splt[0] == "inacl6_add":
            writeInAclRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "inacl6_mod":
            writeInAclRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "inacl6_del":
            writeInAclRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue

        if splt[0] == "outacl4_add":
            writeOutAclRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "outacl4_mod":
            writeOutAclRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "outacl4_del":
            writeOutAclRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue

        if splt[0] == "outacl6_add":
            writeOutAclRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "outacl6_mod":
            writeOutAclRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "outacl6_del":
            writeOutAclRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue

        if splt[0] == "natcfg4_add":
            writeNatCfgRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "natcfg4_mod":
            writeNatCfgRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "natcfg4_del":
            writeNatCfgRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue

        if splt[0] == "natcfg6_add":
            writeNatCfgRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "natcfg6_mod":
            writeNatCfgRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue
        if splt[0] == "natcfg6_del":
            writeNatCfgRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]))
            continue

        if splt[0] == "nattrns4_add":
            writeNatTrnsRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue
        if splt[0] == "nattrns4_mod":
            writeNatTrnsRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue
        if splt[0] == "nattrns4_del":
            writeNatTrnsRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue

        if splt[0] == "nattrns6_add":
            writeNatTrnsRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue
        if splt[0] == "nattrns6_mod":
            writeNatTrnsRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue
        if splt[0] == "nattrns6_del":
            writeNatTrnsRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue

        if splt[0] == "nattrns6_add":
            writeNatTrnsRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]),splt[11],int(splt[12]))
            continue
        if splt[0] == "nattrns6_mod":
            writeNatTrnsRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]),splt[11],int(splt[12]))
            continue
        if splt[0] == "nattrns6_del":
            writeNatTrnsRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]),splt[11],int(splt[12]))
            continue

        if splt[0] == "pbr4norm_add":
            writePbrNormRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr4norm_mod":
            writePbrNormRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr4norm_del":
            writePbrNormRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if splt[0] == "pbr6norm_add":
            writePbrNormRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr6norm_mod":
            writePbrNormRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr6norm_del":
            writePbrNormRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if splt[0] == "pbr4vrf_add":
            writePbrVrfRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr4vrf_mod":
            writePbrVrfRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr4vrf_del":
            writePbrVrfRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if splt[0] == "pbr6vrf_add":
            writePbrVrfRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr6vrf_mod":
            writePbrVrfRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr6vrf_del":
            writePbrVrfRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if splt[0] == "pbr4hop_add":
            writePbrHopRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr4hop_mod":
            writePbrHopRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr4hop_del":
            writePbrHopRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if splt[0] == "pbr6hop_add":
            writePbrHopRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr6hop_mod":
            writePbrHopRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "pbr6hop_del":
            writePbrHopRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue


        if splt[0] == "pbr4lab_add":
            writePbrLabRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "pbr4lab_mod":
            writePbrLabRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "pbr4lab_del":
            writePbrLabRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if splt[0] == "pbr6lab_add":
            writePbrLabRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "pbr6lab_mod":
            writePbrLabRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "pbr6lab_del":
            writePbrLabRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if splt[0] == "inqos4_add":
            writeInQosRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "inqos4_mod":
            writeInQosRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "inqos4_del":
            writeInQosRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue

        if splt[0] == "inqos6_add":
            writeInQosRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "inqos6_mod":
            writeInQosRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "inqos6_del":
            writeInQosRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue

        if splt[0] == "outqos4_add":
            writeOutQosRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "outqos4_mod":
            writeOutQosRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "outqos4_del":
            writeOutQosRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue

        if splt[0] == "outqos6_add":
            writeOutQosRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "outqos6_mod":
            writeOutQosRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue
        if splt[0] == "outqos6_del":
            writeOutQosRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]))
            continue

        if splt[0] == "inqos_add":
            writeInQosRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "inqos_mod":
            writeInQosRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "inqos_del":
            writeInQosRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if splt[0] == "outqos_add":
            writeOutQosRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "outqos_mod":
            writeOutQosRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "outqos_del":
            writeOutQosRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if splt[0] == "flowspec4_add":
            writeFlowspecRules4(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "flowspec4_mod":
            writeFlowspecRules4(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "flowspec4_del":
            writeFlowspecRules4(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if splt[0] == "flowspec6_add":
            writeFlowspecRules6(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "flowspec6_mod":
            writeFlowspecRules6(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue
        if splt[0] == "flowspec6_del":
            writeFlowspecRules6(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if splt[0] == "cpulabel_add":
            writeCpuMplsRules(1,p4info_helper,sw1,int(splt[1]))
            continue
        if splt[0] == "cpulabel_mod":
            writeCpuMplsRules(2,p4info_helper,sw1,int(splt[1]))
            continue
        if splt[0] == "cpulabel_del":
            writeCpuMplsRules(3,p4info_helper,sw1,int(splt[1]))
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

        if splt[0] == "unlabel4_add":
            writeUnMplsRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "unlabel4_mod":
            writeUnMplsRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "unlabel4_del":
            writeUnMplsRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
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

        if splt[0] == "nshfwd_add":
            writeNshFwdRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],int(splt[6]),int(splt[7]))
            continue
        if splt[0] == "nshfwd_mod":
            writeNshFwdRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],int(splt[6]),int(splt[7]))
            continue
        if splt[0] == "nshfwd_del":
            writeNshFwdRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],int(splt[6]),int(splt[7]))
            continue

        if splt[0] == "nshloc_add":
            writeNshLocRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "nshloc_mod":
            writeNshLocRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "nshloc_del":
            writeNshLocRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if splt[0] == "neigh4_add":
            writeNexthopRules(1,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
            writeNeighborRules4(1,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh4_mod":
            writeNexthopRules(2,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
            writeNeighborRules4(2,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh4_del":
            writeNexthopRules(3,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
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

        if splt[0] == "nhop2port_add":
            writeNhop2portRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "nhop2port_mod":
            writeNhop2portRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "nhop2port_del":
            writeNhop2portRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if splt[0] == "portvlan_add":
            writeVlanRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "portvlan_mod":
            writeVlanRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "portvlan_del":
            writeVlanRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if splt[0] == "bundlevlan_add":
            writeBunVlanRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "bundlevlan_mod":
            writeBunVlanRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "bundlevlan_del":
            writeBunVlanRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if splt[0] == "portbundle_add":
            writeBundleRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "portbundle_mod":
            writeBundleRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue
        if splt[0] == "portbundle_del":
            writeBundleRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if splt[0] == "hairpin_add":
            writeHairpinRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "hairpin_mod":
            writeHairpinRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "hairpin_del":
            writeHairpinRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if splt[0] == "pppoe_add":
            writePppoeRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],splt[7])
            continue
        if splt[0] == "pppoe_mod":
            writePppoeRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],splt[7])
            continue
        if splt[0] == "pppoe_del":
            writePppoeRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],splt[7])
            continue

        if splt[0] == "gre4_add":
            writeGre4rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "gre4_mod":
            writeGre4rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "gre4_del":
            writeGre4rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if splt[0] == "gre6_add":
            writeGre6rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "gre6_mod":
            writeGre6rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "gre6_del":
            writeGre6rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue


        if splt[0] == "ipip4_add":
            writeIpip4rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "ipip4_mod":
            writeIpip4rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "ipip4_del":
            writeIpip4rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if splt[0] == "ipip6_add":
            writeIpip6rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "ipip6_mod":
            writeIpip6rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue
        if splt[0] == "ipip6_del":
            writeIpip6rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue


        if splt[0] == "l2tp4_add":
            writeL2tp4rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "l2tp4_mod":
            writeL2tp4rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "l2tp4_del":
            writeL2tp4rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if splt[0] == "l2tp6_add":
            writeL2tp6rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "l2tp6_mod":
            writeL2tp6rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "l2tp6_del":
            writeL2tp6rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if splt[0] == "bridgevxlan4_add":
            writeVxlan4rules(1,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue
        if splt[0] == "bridgevxlan4_mod":
            writeVxlan4rules(2,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue
        if splt[0] == "bridgevxlan4_del":
            writeVxlan4rules(3,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue

        if splt[0] == "bridgevxlan6_add":
            writeVxlan6rules(1,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue
        if splt[0] == "bridgevxlan6_mod":
            writeVxlan6rules(2,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue
        if splt[0] == "bridgevxlan6_del":
            writeVxlan6rules(3,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue

        if splt[0] == "bridgepckoudp4_add":
            writePckoudp4rules(1,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue
        if splt[0] == "bridgepckoudp4_mod":
            writePckoudp4rules(2,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue
        if splt[0] == "bridgepckoudp4_del":
            writePckoudp4rules(3,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue

        if splt[0] == "bridgepckoudp6_add":
            writePckoudp6rules(1,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue
        if splt[0] == "bridgepckoudp6_mod":
            writePckoudp6rules(2,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue
        if splt[0] == "bridgepckoudp6_del":
            writePckoudp6rules(3,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue


        if splt[0] == "xconnect_add":
            writeXconnRules(1,p4info_helper,sw1,int(splt[1]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "xconnect_mod":
            writeXconnRules(2,p4info_helper,sw1,int(splt[1]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "xconnect_del":
            writeXconnRules(3,p4info_helper,sw1,int(splt[1]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if splt[0] == "portbridge_add":
            writeBrprtRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "portbridge_mod":
            writeBrprtRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "portbridge_del":
            writeBrprtRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if splt[0] == "bridgemac_add":
            writeBrmacRules(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue
        if splt[0] == "bridgemac_mod":
            writeBrmacRules(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue
        if splt[0] == "bridgemac_del":
            writeBrmacRules(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue

        if splt[0] == "routedmac_add":
            writeRoumacRules(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]))
            continue
        if splt[0] == "routedmac_mod":
            writeRoumacRules(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]))
            continue
        if splt[0] == "routedmac_del":
            writeRoumacRules(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]))
            continue

        if splt[0] == "bridgelabel_add":
            writeBrlabRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "bridgelabel_mod":
            writeBrlabRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "bridgelabel_del":
            writeBrlabRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if splt[0] == "bridgevpls_add":
            writeBrvplsRules(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "bridgevpls_mod":
            writeBrvplsRules(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),int(splt[5]),int(splt[6]))
            continue
        if splt[0] == "bridgevpls_del":
            writeBrvplsRules(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if splt[0] == "bridgesrv_add":
            writeBrsrvRules(1,p4info_helper,sw1,int(splt[2]),splt[3],int(splt[1]))
            continue
        if splt[0] == "bridgesrv_mod":
            writeBrsrvRules(2,p4info_helper,sw1,int(splt[2]),splt[3],int(splt[1]))
            continue
        if splt[0] == "bridgesrv_del":
            writeBrsrvRules(3,p4info_helper,sw1,int(splt[2]),splt[3],int(splt[1]))
            continue

        if splt[0] == "bridgesrv6_add":
            writeBrsrv6rules(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),splt[5])
            continue
        if splt[0] == "bridgesrv6_mod":
            writeBrsrv6rules(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),splt[5])
            continue
        if splt[0] == "bridgesrv6_del":
            writeBrsrv6rules(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),splt[5])
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
            writeGlobRules6(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue
        if splt[0] == "labroute6_mod":
            addr = splt[1].split("/");
            writeGlobRules6(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue
        if splt[0] == "labroute6_del":
            addr = splt[1].split("/");
            writeGlobRules6(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue

        if splt[0] == "srvroute6_add":
            addr = splt[1].split("/");
            writeSrvRules6(1,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
            continue
        if splt[0] == "srvroute6_mod":
            addr = splt[1].split("/");
            writeSrvRules6(2,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
            continue
        if splt[0] == "srvroute6_del":
            addr = splt[1].split("/");
            writeSrvRules6(3,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
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

        if splt[0] == "unlabel6_add":
            writeUnMplsRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "unlabel6_mod":
            writeUnMplsRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue
        if splt[0] == "unlabel6_del":
            writeUnMplsRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if splt[0] == "mysrv4_add":
            writeMySrv4rules(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue
        if splt[0] == "mysrv4_mod":
            writeMySrv4rules(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue
        if splt[0] == "mysrv4_del":
            writeMySrv4rules(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue

        if splt[0] == "mysrv6_add":
            writeMySrv6rules(1,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue
        if splt[0] == "mysrv6_mod":
            writeMySrv6rules(2,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue
        if splt[0] == "mysrv6_del":
            writeMySrv6rules(3,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
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
            writeNexthopRules(1,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
            writeNeighborRules6(1,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh6_mod":
            writeNexthopRules(2,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
            writeNeighborRules6(2,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue
        if splt[0] == "neigh6_del":
            writeNexthopRules(3,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
            writeNeighborRules6(3,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue

        if splt[0] == "mlocal4_add":
            writeMlocal4rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue
        if splt[0] == "mlocal4_mod":
            writeMlocal4rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue
        if splt[0] == "mlocal4_del":
            writeMlocal4rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue

        if splt[0] == "mlocal6_add":
            writeMlocal6rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue
        if splt[0] == "mlocal6_mod":
            writeMlocal6rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue
        if splt[0] == "mlocal6_del":
            writeMlocal6rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue

        if splt[0] == "mroute4_add":
            writeMroute4rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue
        if splt[0] == "mroute4_mod":
            writeMroute4rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue
        if splt[0] == "mroute4_del":
            writeMroute4rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue

        if splt[0] == "mroute6_add":
            writeMroute6rules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue
        if splt[0] == "mroute6_mod":
            writeMroute6rules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue
        if splt[0] == "mroute6_del":
            writeMroute6rules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue

        if splt[0] == "mlabroute4_add":
            writeMlabRouteRules(1,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue
        if splt[0] == "mlabroute4_mod":
            writeMlabRouteRules(2,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue
        if splt[0] == "mlabroute4_del":
            writeMlabRouteRules(3,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue

        if splt[0] == "mlabroute6_add":
            writeMlabRouteRules(1,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue
        if splt[0] == "mlabroute6_mod":
            writeMlabRouteRules(2,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue
        if splt[0] == "mlabroute6_del":
            writeMlabRouteRules(3,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue

        if splt[0] == "duplabel4_add":
            writeDupLabelRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue
        if splt[0] == "duplabel4_mod":
            writeDupLabelRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue
        if splt[0] == "duplabel4_del":
            writeDupLabelRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue

        if splt[0] == "duplabel6_add":
            writeDupLabelRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue
        if splt[0] == "duplabel6_mod":
            writeDupLabelRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue
        if splt[0] == "duplabel6_del":
            writeDupLabelRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue

        if splt[0] == "duplabloc4_add":
            writeDupLabLocRules(1,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue
        if splt[0] == "duplabloc4_mod":
            writeDupLabLocRules(2,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue
        if splt[0] == "duplabloc4_del":
            writeDupLabLocRules(3,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue

        if splt[0] == "duplabloc6_add":
            writeDupLabLocRules(1,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue
        if splt[0] == "duplabloc6_mod":
            writeDupLabLocRules(2,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue
        if splt[0] == "duplabloc6_del":
            writeDupLabLocRules(3,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue

        if splt[0] == "bierlabel4_add":
            writeBierLabelRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue
        if splt[0] == "bierlabel4_mod":
            writeBierLabelRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue
        if splt[0] == "bierlabel4_del":
            writeBierLabelRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue

        if splt[0] == "bierlabel6_add":
            writeBierLabelRules(1,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue
        if splt[0] == "bierlabel6_mod":
            writeBierLabelRules(2,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue
        if splt[0] == "bierlabel6_del":
            writeBierLabelRules(3,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue

        if splt[0] == "bierlabloc4_add":
            writeBierLabLocRules(1,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "bierlabloc4_mod":
            writeBierLabLocRules(2,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "bierlabloc4_del":
            writeBierLabLocRules(3,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if splt[0] == "bierlabloc6_add":
            writeBierLabLocRules(1,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "bierlabloc6_mod":
            writeBierLabLocRules(2,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue
        if splt[0] == "bierlabloc6_del":
            writeBierLabLocRules(3,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if splt[0] == "mbierroute4_add":
            writeMbierRouteRules(1,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "mbierroute4_mod":
            writeMbierRouteRules(2,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "mbierroute4_del":
            writeMbierRouteRules(3,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if splt[0] == "mbierroute6_add":
            writeMbierRouteRules(1,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "mbierroute6_mod":
            writeMbierRouteRules(2,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue
        if splt[0] == "mbierroute6_del":
            writeMbierRouteRules(3,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
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
            default="127.0.0.1:9559")
    parser.add_argument('--freerouter_address', help='freerouter address',
            type=str, action="store", required=False,
            default="127.0.0.1")
    parser.add_argument('--freerouter_port', help='freerouter port',
            type=str, action="store", required=False,
            default="9080")
    args = parser.parse_args()

    if not os.path.exists(args.p4info):
        parser.print_help()
        print("p4info file not found.")
        parser.exit(1)
    if not os.path.exists(args.bmv2_json):
        parser.print_help()
        print("BMv2 JSON file not found.")
        parser.exit(1)

    main(args.p4info, args.bmv2_json, args.p4runtime_address, args.freerouter_address, args.freerouter_port)
