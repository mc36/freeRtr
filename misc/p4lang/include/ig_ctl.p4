/*
 * Copyright 2019-present GT RARE project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed On an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef _INGRESS_CONTROL_P4_
#define _INGRESS_CONTROL_P4_


control ig_ctl(inout headers hdr,
               inout ingress_metadata_t ig_md,
               inout standard_metadata_t ig_intr_md) {

    IngressControlARP() ig_ctl_arp;
    IngressControlPPPOE() ig_ctl_pppoe;
    IngressControlMPLS() ig_ctl_mpls;
    IngressControlBridge() ig_ctl_bridge;
    IngressControlIPv4() ig_ctl_ipv4;
    IngressControlIPv6() ig_ctl_ipv6;
    IngressControlIPv4b() ig_ctl_ipv4b;
    IngressControlIPv6b() ig_ctl_ipv6b;
    IngressControlVlanIn() ig_ctl_vlan_in;
    IngressControlVRF() ig_ctl_vrf;
    IngressControlLLC() ig_ctl_llc;
    IngressControlCoPP() ig_ctl_copp;
    IngressControlTunnel() ig_ctl_tunnel;
    IngressControlAclIn() ig_ctl_acl_in;
    IngressControlNAT() ig_ctl_nat;
    IngressControlPBR() ig_ctl_pbr;
    IngressControlQosIn() ig_ctl_qos_in;
    IngressControlFlowspec() ig_ctl_flowspec;
    IngressControlMcast() ig_ctl_mcast;
    IngressControlOutPort() ig_ctl_outport;
    IngressControlBundle() ig_ctl_bundle;

    counter((MAX_PORT+1), CounterType.packets_and_bytes) pkt_out_stats;

    apply {
        if (ig_intr_md.ingress_port == CPU_PORT) {
            pkt_out_stats.count((bit<32>)ig_md.source_id);
            /*
             * pkt received from the controlled has a cpu header
             * that containes egress port id. Once retrieve
             * we remove the cpu header (setInvalid)
             * So it will not be taken into accoiunt by deparser
             */
            ig_intr_md.egress_spec = (PortId_t)hdr.cpu.port;
            hdr.cpu.setInvalid();
            ig_md.punting = 1;
            return;
        }
        /*
         * So it is a dataplane packet
         */
        hdr.cpu.setInvalid();
        ig_ctl_vlan_in.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_pppoe.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_acl_in.apply(hdr,ig_md,ig_intr_md);
        if (ig_md.dropping == 1) {
            return;
        }
        ig_ctl_qos_in.apply(hdr,ig_md,ig_intr_md);
        if (ig_md.dropping == 1) {
            return;
        }
        ig_ctl_vrf.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_arp.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_llc.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_mpls.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_flowspec.apply(hdr,ig_md,ig_intr_md);
        if (ig_md.dropping == 1) {
            return;
        }
        ig_ctl_nat.apply(hdr,ig_md,ig_intr_md);
        if ( ig_md.dropping == 1) {
            hdr.cpu.setValid();
            hdr.cpu.port = ig_md.ingress_id;
            ig_intr_md.egress_spec = CPU_PORT;
            ig_md.punting = 1;
            return;
        }
        ig_ctl_pbr.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_ipv4.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_ipv6.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_bridge.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_ipv4b.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_ipv6b.apply(hdr,ig_md,ig_intr_md);
        if ( ig_md.nexthop_id == CPU_PORT) {
            ig_ctl_tunnel.apply(hdr,ig_md,ig_intr_md);
            if (ig_md.need_recir == 1) {
                if (hdr.vlan.isValid()) hdr.vlan.setInvalid();
                if (hdr.pppoeD.isValid()) hdr.pppoeD.setInvalid();
                if (hdr.pppoeB.isValid()) hdr.pppoeB.setInvalid();
                if (hdr.l2tpbr.isValid()) hdr.l2tpbr.setInvalid();
                return;
            }
            ig_ctl_mcast.apply(hdr,ig_md,ig_intr_md);
            if (ig_md.dropping == 1) {
                return;
            }
            if (ig_md.need_clone == 1) {
                if (hdr.vlan.isValid()) hdr.vlan.setInvalid();
                if (hdr.pppoeD.isValid()) hdr.pppoeD.setInvalid();
                if (hdr.pppoeB.isValid()) hdr.pppoeB.setInvalid();
                if (hdr.l2tpbr.isValid()) hdr.l2tpbr.setInvalid();
                return;
            }
            ig_ctl_copp.apply(hdr,ig_md,ig_intr_md);
            if (ig_md.dropping == 1) {
                return;
            }
            if (hdr.pppoeB.isValid() && hdr.eth5.isValid() && hdr.eth6.isValid()) {
                hdr.ethernet = hdr.eth5;
                hdr.eth5.setInvalid();
            }
            hdr.cpu.setValid();
            hdr.cpu.port = ig_md.ingress_id;
            ig_intr_md.egress_spec = CPU_PORT;
            ig_md.punting = 1;
            return;
        }

        if (hdr.vlan.isValid()) hdr.vlan.setInvalid();
        if (hdr.pppoeD.isValid()) hdr.pppoeD.setInvalid();
        if (hdr.pppoeB.isValid()) hdr.pppoeB.setInvalid();
        if (hdr.l2tpbr.isValid()) hdr.l2tpbr.setInvalid();
        ig_ctl_outport.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_bundle.apply(hdr,ig_md,ig_intr_md);
    }
}

#endif // _INGRESS_CONTROL_P4_
