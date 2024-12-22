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
            "hdr.vlan.vid": vlan,
            "hdr.vlanq.vid": 0
        },
        action_name="ig_ctl.ig_ctl_vlan_in.act_set_vlan_iface",
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
        action_name="ig_ctl.ig_ctl_outport.act_set_port_vlan",
        action_params={
            "port": main,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeQinqRules(delete, p4info_helper, ingress_sw, port, main, outer, inner):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vlan_in.tbl_vlan_in",
        match_fields={
            "ig_md.ingress_id": main,
            "hdr.vlan.vid": outer,
            "hdr.vlanq.vid": inner
        },
        action_name="ig_ctl.ig_ctl_vlan_in.act_set_qinq_iface",
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
        action_name="eg_ctl.eg_ctl_vlan_out.act_set_qinq_port",
        action_params={
            "port": main,
            "outer": outer,
            "inner": inner
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
        action_name="ig_ctl.ig_ctl_outport.act_set_port_vlan",
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
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_outport.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nhop,
        },
        action_name="eg_ctl.eg_ctl_outport.act_set_port_nexthop",
        action_params={
            "subif": subif,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_outport.tbl_nexthop",
        match_fields={
            "ig_md.nexthop_id": nhop,
        },
        action_name="ig_ctl.ig_ctl_outport.act_set_port_nexthop",
        action_params={
            "port": port,
            "subif": subif,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)
    table_entry4 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_nexthop",
        match_fields={
            "neigh": nhop,
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_set_port_nexthop",
        action_params={
            "subif": subif,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry4, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry4, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry4, False)
    table_entry5 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_nexthop",
        match_fields={
            "neigh": nhop,
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_set_port_nexthop",
        action_params={
            "subif": subif,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry5, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry5, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry5, False)


def writeBunVlanRules(delete, p4info_helper, ingress_sw, main, vlan, port):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vlan_in.tbl_vlan_in",
        match_fields={
            "ig_md.ingress_id": main,
            "hdr.vlan.vid": vlan,
            "hdr.vlanq.vid": 0
        },
        action_name="ig_ctl.ig_ctl_vlan_in.act_set_vlan_iface",
        action_params={
            "src": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeBunQinqRules(delete, p4info_helper, ingress_sw, main, outer, inner, port):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vlan_in.tbl_vlan_in",
        match_fields={
            "ig_md.ingress_id": main,
            "hdr.vlan.vid": outer,
            "hdr.vlanq.vid": inner
        },
        action_name="ig_ctl.ig_ctl_vlan_in.act_set_qinq_iface",
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



def writeTmux4rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 18,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_tmux",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_tmux4",
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


def writeTmux6rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 18,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_tmux",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_tmux6",
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


def writeL3tp4rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, tid):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv4.protocol": 115,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_l3tp",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_l3tp4",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
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


def writeL3tp6rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, tid):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_tunnel.tbl_tunnel6",
        match_fields={
            "ig_md.vrf": vrf,
            "hdr.ipv6.next_hdr": 115,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_l3tp",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_l3tp6",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
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



def writeAmt4rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt):
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
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_amt",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_amt4",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
            "src_port": sprt,
            "dst_port": dprt,
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


def writeAmt6rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt):
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
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_amt",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_amt6",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
            "src_port": sprt,
            "dst_port": dprt,
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



def writeGtp4rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt, teid):
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
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_gtp",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_gtp4",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
            "src_port": sprt,
            "dst_port": dprt,
            "teid": teid,
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


def writeGtp6rules(delete, p4info_helper, ingress_sw, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt, teid):
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
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_gtp",
        action_params={
            "port": port
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_gtp6",
        action_params={
            "dst_mac_addr": dmac,
            "src_mac_addr": smac,
            "egress_port": phport,
            "acl_port": port,
            "src_ip_addr": sip,
            "dst_ip_addr": dip,
            "src_port": sprt,
            "dst_port": dprt,
            "teid": teid,
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





def writeEtherip4rules(delete, p4info_helper, ingress_sw, bridge, addr, sip, dip, nexthop, vrf, port):
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
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_etherip4",
        action_params={
            "nexthop": nexthop,
            "dst_ip_addr": dip,
            "src_ip_addr": sip,
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
            "hdr.ipv4.protocol": 97,
            "hdr.ipv4.src_addr": dip,
            "hdr.ipv4.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_etherip",
        action_params={
            "port": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeEtherip6rules(delete, p4info_helper, ingress_sw, bridge, addr, sip, dip, nexthop, vrf, port):
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
        action_name="ig_ctl.ig_ctl_bridge.act_set_bridge_etherip6",
        action_params={
            "nexthop": nexthop,
            "dst_ip_addr": dip,
            "src_ip_addr": sip,
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
            "hdr.ipv6.next_hdr": 97,
            "hdr.ipv6.src_addr": dip,
            "hdr.ipv6.dst_addr": sip,
            "ig_md.layer4_srcprt": 0,
            "ig_md.layer4_dstprt": 0,
        },
        action_name="ig_ctl.ig_ctl_tunnel.act_tunnel_etherip",
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


def writeLoconnIfcRules(delete, p4info_helper, ingress_sw, port, target):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vrf.tbl_vrf",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_vrf.act_set_loconn_ifc",
        action_params={
            "port": target,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)


def writeLoconnNeiRules(delete, p4info_helper, ingress_sw, port, target):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vrf.tbl_vrf",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_vrf.act_set_loconn_nei",
        action_params={
            "nhop": target,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)


def writeNshConnRules(delete, p4info_helper, ingress_sw, port, sp, si):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_vrf.tbl_vrf",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_vrf.act_set_nshconn",
        action_params={
            "sp": sp,
            "si": si,
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_ipv4_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_ipv6_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_ipv4_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_ipv6_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_ipv4_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_ipv6_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_ipv4_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_ipv6_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeDropRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4.act_ipv4_fib_discard",
        action_params={})
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4b.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4b.act_ipv4_fib_discard",
        action_params={})
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_set_drop",
        action_params={})
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeDropRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, vrf):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6.act_ipv6_fib_discard",
        action_params={})
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6b.dst_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6b.act_ipv6_fib_discard",
        action_params={})
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_set_drop",
        action_params={})
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


def add2dictIfNot(dic, key, val, msk, cnd):
    if msk == cnd:
        return;
    dic[key] = (val,msk)



def writeVerifyRules4(delete, p4info_helper, ingress_sw, port, mode):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_port_verify",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_ipv4_cpl_set_mode",
        action_params={
            "mod": mode
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeVerifyRules6(delete, p4info_helper, ingress_sw, port, mode):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_port_verify",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_ipv6_cpl_set_mode",
        action_params={
            "mod": mode
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)



def writeSgtTagRules(delete, p4info_helper, ingress_sw, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_sgt.tbl_sgt_in",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_sgt.act_need_tag",
        action_params={
        })
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_sgt.tbl_sgt_out",
        match_fields={
            "eg_md.aclport_id": port
        },
        action_name="eg_ctl.eg_ctl_sgt.act_need_tag",
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


def writeSgtSetRules(delete, p4info_helper, ingress_sw, port, group):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_sgt.tbl_sgt_set",
        match_fields={
            "ig_md.source_id": port
        },
        action_name="ig_ctl.ig_ctl_sgt.act_set_tag",
        action_params={
            "group": group
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeCoppRules4(delete, p4info_helper, ingress_sw, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeCoppRules6(delete, p4info_helper, ingress_sw, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeInAclRules4(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeInAclRules6(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeOutAclRules4(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_out.tbl_ipv4_acl",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_acl_out.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeOutAclRules6(delete, p4info_helper, ingress_sw, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_out.tbl_ipv6_acl",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_acl_out.act_"+act,
        priority=65535-pri,
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeNatCfgRules4(delete, p4info_helper, ingress_sw, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeNatCfgRules6(delete, p4info_helper, ingress_sw, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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
    if proto == 6:
        name = "tcp"
    elif proto == 17:
        name = "udp"
    else:
        name = "oth"
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
        action_name="ig_ctl.ig_ctl_nat.act_rewrite_ipv4"+name,
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
    if proto == 6:
        name = "tcp"
    elif proto == 17:
        name = "udp"
    else:
        name = "oth"
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
        action_name="ig_ctl.ig_ctl_nat.act_rewrite_ipv6"+name,
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


def writeInspectRules4(delete, p4info_helper, ingress_sw, port, proto, sa, sp, ta, tp):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_in.tbl_ipv4_insp",
        match_fields={
            "ig_md.source_id": port,
            "hdr.ipv4.protocol": proto,
            "hdr.ipv4.src_addr": sa,
            "hdr.ipv4.dst_addr": ta,
            "ig_md.layer4_srcprt": sp,
            "ig_md.layer4_dstprt": tp
        },
        action_name="ig_ctl.ig_ctl_acl_in.act_permit",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_out.tbl_ipv4_insp",
        match_fields={
            "ig_md.aclport_id": port,
            "hdr.ipv4.protocol": proto,
            "hdr.ipv4.src_addr": ta,
            "hdr.ipv4.dst_addr": sa,
            "ig_md.layer4_srcprt": tp,
            "ig_md.layer4_dstprt": sp
        },
        action_name="ig_ctl.ig_ctl_acl_out.act_permit",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writeInspectRules6(delete, p4info_helper, ingress_sw, port, proto, sa, sp, ta, tp):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_in.tbl_ipv6_insp",
        match_fields={
            "ig_md.source_id": port,
            "hdr.ipv6.next_hdr": proto,
            "hdr.ipv6.src_addr": sa,
            "hdr.ipv6.dst_addr": ta,
            "ig_md.layer4_srcprt": sp,
            "ig_md.layer4_dstprt": tp
        },
        action_name="ig_ctl.ig_ctl_acl_in.act_permit",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_acl_out.tbl_ipv6_insp",
        match_fields={
            "ig_md.aclport_id": port,
            "hdr.ipv6.next_hdr": proto,
            "hdr.ipv6.src_addr": ta,
            "hdr.ipv6.dst_addr": sa,
            "ig_md.layer4_srcprt": tp,
            "ig_md.layer4_dstprt": sp
        },
        action_name="ig_ctl.ig_ctl_acl_out.act_permit",
        action_params={
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry2, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry2, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry2, False)


def writePbrNormRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writePbrNormRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writePbrVrfRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writePbrVrfRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writePbrHopRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writePbrHopRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writePbrLabRules4(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, tlab, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writePbrLabRules6(delete, p4info_helper, ingress_sw, vrf, tvrf, thop, tlab, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeInQosRules4(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeInQosRules6(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.source_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeOutQosRules4(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_qos_out.tbl_ipv4_qos",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_qos_out.act_"+act,
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


def writeOutQosRules6(delete, p4info_helper, ingress_sw, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.aclport_id": port}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_qos_out.tbl_ipv6_qos",
        match_fields=matches,
        action_name="ig_ctl.ig_ctl_qos_out.act_"+act,
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
    metid = p4info_helper.get_meters_id("ig_ctl.ig_ctl_qos_out.policer")
    if delete != 3:
        ingress_sw.WriteMeter(metid, (meter+1), bytes, bytes)




def writeFlowspecRules4(delete, p4info_helper, ingress_sw, vrf, meter, bytes, interval, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv4.protocol",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv4.src_addr",sa,sam,"0.0.0.0")
    add2dictIfNot(matches, "hdr.ipv4.dst_addr",da,dam,"0.0.0.0")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv4.diffserv",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv4.identification",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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


def writeFlowspecRules6(delete, p4info_helper, ingress_sw, vrf, meter, bytes, interval, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm, gr, grm):
    matches={"ig_md.vrf": vrf}
    add2dictIfNot(matches, "hdr.ipv6.next_hdr",pr,prm,0)
    add2dictIfNot(matches, "hdr.ipv6.src_addr",sa,sam,"::")
    add2dictIfNot(matches, "hdr.ipv6.dst_addr",da,dam,"::")
    add2dictIfNot(matches, "ig_md.layer4_srcprt",sp,spm,0)
    add2dictIfNot(matches, "ig_md.layer4_dstprt",dp,dpm,0)
    add2dictIfNot(matches, "hdr.ipv6.traffic_class",ts,tsm,0)
    add2dictIfNot(matches, "hdr.ipv6.flow_label",fl,flm,0)
    add2dictIfNot(matches, "ig_md.sec_grp_id",gr,grm,0)
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



def writeMyaddrRules4(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf):
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
    if port < 0:
        return
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_ipv4_cpl_ours",
        action_params={
            "subif": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


def writeMyaddrRules6(delete, p4info_helper, ingress_sw, dst_ip_addr, dst_net_mask, port, vrf):
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
    if port < 0:
        return
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.src_addr": (dst_ip_addr,dst_net_mask),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_ipv6_cpl_ours",
        action_params={
                "subif": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry3, False)


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


def writePwheNhRules(delete, p4info_helper, ingress_sw, nexthop, dst_mac_addr, src_mac_addr, port, aclport, core_dst_mac, core_src_mac, label, vpnlab):
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_nexthop.tbl_nexthop",
        match_fields={
            "eg_md.nexthop_id": nexthop,
        },
        action_name="eg_ctl.eg_ctl_nexthop.act_ipv4_pwhe",
        action_params={
            "dst_mac_addr": dst_mac_addr,
            "src_mac_addr": src_mac_addr,
            "egress_port": port,
            "acl_port": aclport,
            "core_dst_mac": core_dst_mac,
            "core_src_mac": core_src_mac,
            "egress_label": label,
            "vpn_label": vpnlab
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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv4c.tbl_ipv4_fib_lpm",
        match_fields={
            "hdr.ipv4.src_addr": (dst_ip_addr,32),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv4c.act_ipv4_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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
    table_entry3 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_ipv6c.tbl_ipv6_fib_lpm",
        match_fields={
            "hdr.ipv6.src_addr": (dst_ip_addr,128),
            "ig_md.vrf": vrf
        },
        action_name="ig_ctl.ig_ctl_ipv6c.act_ipv6_cpl_found",
        action_params={
            "hop": port
        })
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry1, False)
        ingress_sw.WriteTableEntry(table_entry2, False)
        ingress_sw.WriteTableEntry(table_entry3, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry1, False)
        ingress_sw.ModifyTableEntry(table_entry2, False)
        ingress_sw.ModifyTableEntry(table_entry3, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry1, False)
        ingress_sw.DeleteTableEntry(table_entry2, False)
        ingress_sw.DeleteTableEntry(table_entry3, False)


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


def writeVpnMplsRules(delete, p4info_helper, ingress_sw, dst_label, new_label1, new_label2, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_swap2_set_nexthop",
        action_params={
            "egress_label1": new_label1,
            "egress_label2": new_label2,
            "nexthop_id": port
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_swap2_set_nexthop",
        action_params={
            "egress_label1": new_label1,
            "egress_label2": new_label2,
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


def writePwheMplsRules(delete, p4info_helper, ingress_sw, dst_label, port):
    table_entry1 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
        match_fields={
            "hdr.mpls0.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_pwhe",
        action_params={
            "port": port
        }
    )
    table_entry2 = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_mpls.tbl_mpls_fib_decap",
        match_fields={
            "hdr.mpls1.label": (dst_label)
        },
        action_name="ig_ctl.ig_ctl_mpls.act_mpls_decap_pwhe",
        action_params={
            "port": port
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


def writeNshIfcRules(delete, p4info_helper, ingress_sw, sp, si, prt, src, dst, tsp, tsi):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nsh.tbl_nsh",
        match_fields={
            "hdr.nsh.sp": (sp),
            "hdr.nsh.si": (si)
        },
        action_name="ig_ctl.ig_ctl_nsh.act_fwd_ifc",
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


def writeNshNeiRules(delete, p4info_helper, ingress_sw, sp, si, nei, tsp, tsi):
    table_entry = p4info_helper.buildTableEntry(
        table_name="ig_ctl.ig_ctl_nsh.tbl_nsh",
        match_fields={
            "hdr.nsh.sp": (sp),
            "hdr.nsh.si": (si)
        },
        action_name="ig_ctl.ig_ctl_nsh.act_fwd_nei",
        action_params={
            "nei": nei,
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




def writeMneiRoute4rules(delete, p4info_helper, ingress_sw, vrf, sess, dip, sip, ingr, port, nhop):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":nhop})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": nhop
        },
        action_name="eg_ctl.eg_ctl_mcast.act_neigh",
        action_params={
            "nhop": nhop
        }
    )
    if delete == 1:
        ingress_sw.WriteTableEntry(table_entry, False)
    elif delete == 2:
        ingress_sw.ModifyTableEntry(table_entry, False)
    else:
        ingress_sw.DeleteTableEntry(table_entry, False)


def writeMneiRoute6rules(delete, p4info_helper, ingress_sw, vrf, sess, dip, sip, ingr, port, nhop):
    global mcast
    sess = sess & 0xfff
    if delete != 3:
        mcast.append({"egress_port":port, "instance":nhop})
    table_entry = p4info_helper.buildTableEntry(
        table_name="eg_ctl.eg_ctl_mcast.tbl_mcast",
        match_fields={
            "eg_md.clone_session": sess,
            "eg_intr_md.egress_rid": nhop
        },
        action_name="eg_ctl.eg_ctl_mcast.act_neigh",
        action_params={
            "nhop": nhop
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
    fil = sck.makefile('w')
    fil.write("platform bmv2\r\n");
    fil.write("capabilities copp acl racl inspect nat vlan bundle bridge pppoe hairpin gre l2tp l3tp route mpls vpls evpn eompls gretap pppoetap l2tptap l3tptap vxlan etherip ipip macsec ipsec pckoudp openvpn wireguard srv6 pbr qos flwspc mroute duplab bier nsh sgt amt gtp vrfysrc loconn pwhe\r\n");
    for x in range(0, 10):
        data = "portname %i bmv2-port%i\r\n" % (x,x)
        fil.write(data)
    fil.write("dynrange 512 1023\r\n")
    fil.write("vrfrange 1 1023\r\n")
    fil.write("neirange 4096 65535\r\n");
    fil.write("nomore\r\n")
    for x in range(0, 15):
        data = "state %i 1\r\n" % (x)
        fil.write(data)
    fil.flush()
    fil = sck.makefile('r')

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
        cmds = splt[0].split("_")
        mode = 0;
        if len(cmds) > 1:
            if cmds[1] == "add":
                mode = 1;
            if cmds[1] == "mod":
                mode = 2;
            if cmds[1] == "del":
                mode = 3;


        if cmds[0] == "route4":
            addr = splt[1].split("/");
            writeForwardRules4(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue

        if cmds[0] == "labroute4":
            addr = splt[1].split("/");
            writeGlobRules4(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue

        if cmds[0] == "srvroute4":
            addr = splt[1].split("/");
            writeSrvRules4(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
            continue

        if cmds[0] == "vpnroute4":
            addr = splt[1].split("/");
            writeVpnRules4(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if cmds[0] == "droproute4":
            addr = splt[1].split("/");
            writeDropRules4(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]))
            continue

        if cmds[0] == "myaddr4":
            addr = splt[1].split("/");
            writeMyaddrRules4(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "verify4":
            writeVerifyRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "verify6":
            writeVerifyRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "sgttag":
            writeSgtTagRules(mode,p4info_helper,sw1,int(splt[1]))
            continue

        if cmds[0] == "sgtset":
            writeSgtSetRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "copp4":
            writeCoppRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "copp6":
            writeCoppRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "inacl4":
            writeInAclRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "inacl6":
            writeInAclRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "outacl4":
            writeOutAclRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "outacl6":
            writeOutAclRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "natcfg4":
            writeNatCfgRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "natcfg6":
            writeNatCfgRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),int(splt[5]),splt[6],splt[7],splt[8],splt[9],int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "nattrns4":
            writeNatTrnsRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue

        if cmds[0] == "nattrns6":
            writeNatTrnsRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]),splt[7],int(splt[8]),splt[9],int(splt[10]))
            continue

        if cmds[0] == "inspect4":
            writeInspectRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]))
            continue

        if cmds[0] == "inspect6":
            writeInspectRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],int(splt[4]),splt[5],int(splt[6]))
            continue

        if cmds[0] == "pbr4norm":
            writePbrNormRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]))
            continue

        if cmds[0] == "pbr6norm":
            writePbrNormRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]))
            continue

        if cmds[0] == "pbr4vrf":
            writePbrVrfRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]))
            continue

        if cmds[0] == "pbr6vrf":
            writePbrVrfRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]))
            continue

        if cmds[0] == "pbr4hop":
            writePbrHopRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]))
            continue

        if cmds[0] == "pbr6hop":
            writePbrHopRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[6]),int(splt[7]),splt[8],splt[9],splt[10],splt[11],int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]))
            continue

        if cmds[0] == "pbr4lab":
            writePbrLabRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]),int(splt[22]))
            continue

        if cmds[0] == "pbr6lab":
            writePbrLabRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]),int(splt[22]))
            continue

        if cmds[0] == "inqos4":
            writeInQosRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if cmds[0] == "inqos6":
            writeInQosRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if cmds[0] == "outqos4":
            writeOutQosRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if cmds[0] == "outqos6":
            writeOutQosRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],int(splt[5]),int(splt[6]),splt[7],splt[8],splt[9],splt[10],int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]))
            continue

        if cmds[0] == "inqos":
            writeInQosRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "outqos":
            writeOutQosRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "flowspec4":
            writeFlowspecRules4(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]),int(splt[22]))
            continue

        if cmds[0] == "flowspec6":
            writeFlowspecRules6(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],int(splt[7]),int(splt[8]),splt[9],splt[10],splt[11],splt[12],int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]),int(splt[20]),int(splt[21]),int(splt[22]))
            continue

        if cmds[0] == "cpulabel":
            writeCpuMplsRules(mode,p4info_helper,sw1,int(splt[1]))
            continue

        if cmds[0] == "pwhelab":
            writePwheMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "label4":
            writeMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue

        if cmds[0] == "vpnlabel4":
            writeVpnMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[5]),int(splt[2]))
            continue

        if cmds[0] == "unlabel4":
            writeUnMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "mylabel4":
            writeMyMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "nshifc":
            writeNshIfcRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],int(splt[6]),int(splt[7]))
            continue

        if cmds[0] == "nshnei":
            writeNshNeiRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]))
            continue

        if cmds[0] == "nshloc":
            writeNshLocRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "neigh4":
            writeNexthopRules(mode,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
            writeNeighborRules4(mode,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue

        if cmds[0] == "pwhenei4":
            writePwheNhRules(mode,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]),int(splt[7]),splt[8],splt[9],int(splt[10]),int(splt[11]))
            writeNeighborRules4(mode,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue

        if cmds[0] == "portvrf":
            writeVrfRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "nhop2port":
            writeNhop2portRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "portvlan":
            writeVlanRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "portqinq":
            writeQinqRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue

        if cmds[0] == "bundlevlan":
            writeBunVlanRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "bundleqinq":
            writeBunQinqRules(mode,p4info_helper,sw1,int(splt[4]),int(splt[5]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "portbundle":
            writeBundleRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "hairpin":
            writeHairpinRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "pppoe":
            writePppoeRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),splt[6],splt[7])
            continue

        if cmds[0] == "gre4":
            writeGre4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if cmds[0] == "gre6":
            writeGre6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if cmds[0] == "tmux4":
            writeTmux4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if cmds[0] == "tmux6":
            writeTmux6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if cmds[0] == "ipip4":
            writeIpip4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if cmds[0] == "ipip6":
            writeIpip6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8])
            continue

        if cmds[0] == "l2tp4":
            writeL2tp4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if cmds[0] == "l2tp6":
            writeL2tp6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if cmds[0] == "l3tp4":
            writeL3tp4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]))
            continue

        if cmds[0] == "l3tp6":
            writeL3tp6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]))
            continue

        if cmds[0] == "amt4":
            writeAmt4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]))
            continue

        if cmds[0] == "amt6":
            writeAmt6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]))
            continue

        if cmds[0] == "gtp4":
            writeGtp4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if cmds[0] == "gtp6":
            writeGtp6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],splt[6],int(splt[7]),splt[8],int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if cmds[0] == "bridgevxlan4":
            writeVxlan4rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue

        if cmds[0] == "bridgevxlan6":
            writeVxlan6rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]));
            continue

        if cmds[0] == "bridgeetherip4":
            writeEtherip4rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]));
            continue

        if cmds[0] == "bridgeetherip6":
            writeEtherip6rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]));
            continue

        if cmds[0] == "bridgepckoudp4":
            writePckoudp4rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue

        if cmds[0] == "bridgepckoudp6":
            writePckoudp6rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]));
            continue


        if cmds[0] == "xconnect":
            writeXconnRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if cmds[0] == "loconnifc":
            writeLoconnIfcRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "loconnnei":
            writeLoconnNeiRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "nshconn":
            writeNshConnRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "portbridge":
            writeBrprtRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "bridgemac":
            writeBrmacRules(mode,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue

        if cmds[0] == "routedmac":
            writeRoumacRules(mode,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]),int(splt[4]))
            continue

        if cmds[0] == "bridgelabel":
            writeBrlabRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "bridgevpls":
            writeBrvplsRules(mode,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if cmds[0] == "bridgesrv":
            writeBrsrvRules(mode,p4info_helper,sw1,int(splt[2]),splt[3],int(splt[1]))
            continue

        if cmds[0] == "bridgesrv6":
            writeBrsrv6rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[4]),splt[5])
            continue

        if cmds[0] == "route6":
            addr = splt[1].split("/");
            writeForwardRules6(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]))
            continue

        if cmds[0] == "labroute6":
            addr = splt[1].split("/");
            writeGlobRules6(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]))
            continue

        if cmds[0] == "srvroute6":
            addr = splt[1].split("/");
            writeSrvRules6(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),splt[5])
            continue

        if cmds[0] == "vpnroute6":
            addr = splt[1].split("/");
            writeVpnRules6(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[4]),int(splt[5]),int(splt[6]))
            continue

        if cmds[0] == "droproute6":
            addr = splt[1].split("/");
            writeDropRules6(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]))
            continue

        if cmds[0] == "myaddr6":
            addr = splt[1].split("/");
            writeMyaddrRules6(mode,p4info_helper,sw1,addr[0],int(addr[1]),int(splt[2]),int(splt[3]))
            continue

        if cmds[0] == "label6":
            writeMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[2]))
            continue

        if cmds[0] == "vpnlabel6":
            writeVpnMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[4]),int(splt[5]),int(splt[2]))
            continue

        if cmds[0] == "unlabel6":
            writeUnMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "mysrv4":
            writeMySrv4rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue

        if cmds[0] == "mysrv6":
            writeMySrv6rules(mode,p4info_helper,sw1,int(splt[1]),splt[2],int(splt[3]))
            continue

        if cmds[0] == "mylabel6":
            writeMyMplsRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]))
            continue

        if cmds[0] == "neigh6":
            writeNexthopRules(mode,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]))
            writeNeighborRules6(mode,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue

        if cmds[0] == "pwhenei6":
            writePwheNhRules(mode,p4info_helper,sw1,int(splt[1]),splt[3],splt[5],int(splt[6]),int(splt[7]),splt[8],splt[9],int(splt[10]),int(splt[11]))
            writeNeighborRules6(mode,p4info_helper,sw1,splt[2],int(splt[1]),int(splt[4]))
            continue

        if cmds[0] == "mlocal4":
            writeMlocal4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue

        if cmds[0] == "mlocal6":
            writeMlocal6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),splt[6])
            continue

        if cmds[0] == "mroute4":
            writeMroute4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue

        if cmds[0] == "mroute6":
            writeMroute6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),splt[8],splt[9])
            continue

        if cmds[0] == "mneiroute4":
            writeMneiRoute4rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]))
            continue

        if cmds[0] == "mneiroute6":
            writeMneiRoute6rules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]))
            continue

        if cmds[0] == "mlabroute4":
            writeMlabRouteRules(mode,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue

        if cmds[0] == "mlabroute6":
            writeMlabRouteRules(mode,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]))
            continue

        if cmds[0] == "duplabel4":
            writeDupLabelRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue

        if cmds[0] == "duplabel6":
            writeDupLabelRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]))
            continue

        if cmds[0] == "duplabloc4":
            writeDupLabLocRules(mode,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue

        if cmds[0] == "duplabloc6":
            writeDupLabLocRules(mode,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),splt[4])
            continue

        if cmds[0] == "bierlabel4":
            writeBierLabelRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue

        if cmds[0] == "bierlabel6":
            writeBierLabelRules(mode,p4info_helper,sw1,int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]))
            continue

        if cmds[0] == "bierlabloc4":
            writeBierLabLocRules(mode,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if cmds[0] == "bierlabloc6":
            writeBierLabLocRules(mode,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),int(splt[3]),int(splt[4]),int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]))
            continue

        if cmds[0] == "mbierroute4":
            writeMbierRouteRules(mode,p4info_helper,sw1,"4",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue

        if cmds[0] == "mbierroute6":
            writeMbierRouteRules(mode,p4info_helper,sw1,"6",int(splt[1]),int(splt[2]),splt[3],splt[4],int(splt[5]),int(splt[6]),int(splt[7]),int(splt[8]),int(splt[9]),int(splt[10]),int(splt[11]),int(splt[12]),int(splt[13]),int(splt[14]),int(splt[15]),int(splt[16]),int(splt[17]),int(splt[18]),int(splt[19]))
            continue





if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='P4Runtime Controller')

    parser.add_argument('--p4info', help='p4info proto in text format from p4c',
            type=str, action="store", required=False,
            default="./router.txt")
    parser.add_argument('--bmv2-json', help='BMv2 JSON file from p4c',
            type=str, action="store", required=False,
            default="./router.json")
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
