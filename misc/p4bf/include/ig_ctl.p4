/*
 * Copyright 2019-present GÉANT RARE project
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

control ig_ctl(inout headers hdr, inout ingress_metadata_t ig_md,
               in ingress_intrinsic_metadata_t ig_intr_md,
               in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
               inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
               inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

#ifdef HAVE_NOHW

    apply {
        if (ig_intr_md.ingress_port == CPU_PORT) {
            ig_tm_md.ucast_egress_port =(PortId_t) hdr.cpu.port;
            ig_tm_md.bypass_egress = 1;
            hdr.cpu.setInvalid();
        } else {
            hdr.cpu.setValid();
            hdr.cpu._padding = 0;
            hdr.cpu.port = ig_md.ingress_id;
            ig_tm_md.ucast_egress_port = CPU_PORT;
            ig_tm_md.bypass_egress = 1;
        }
    }

#else


    IngressControlBundle() ig_ctl_bundle;

#ifdef HAVE_MPLS
    IngressControlMPLS()ig_ctl_mpls;
#endif
#ifdef HAVE_POLKA
    IngressControlPOLKA()ig_ctl_polka;
#endif
#ifdef HAVE_NSH
    IngressControlNSH() ig_ctl_nsh;
#endif
#ifdef HAVE_PPPOE
    IngressControlPPPOE() ig_ctl_pppoe;
#endif
    IngressControlPktPreEmit()ig_ctl_pkt_pre_emit;
#ifdef HAVE_BRIDGE
    IngressControlBridge()ig_ctl_bridge;
#endif
    IngressControlIPv4()ig_ctl_ipv4;
    IngressControlIPv6()ig_ctl_ipv6;
#ifdef HAVE_SRV6
    IngressControlIPv4b() ig_ctl_ipv4b;
    IngressControlIPv6b() ig_ctl_ipv6b;
#endif
#ifdef HAVE_TUN
    IngressControlTunnel() ig_ctl_tunnel;
#endif
#ifdef HAVE_COPP
    IngressControlCoPP()ig_ctl_copp;
#endif
#ifdef HAVE_INACL
    IngressControlAclIn() ig_ctl_acl_in;
#endif
    IngressControlVlanIn()ig_ctl_vlan_in;
    IngressControlVRF()ig_ctl_vrf;
#ifdef HAVE_NAT
    IngressControlNAT() ig_ctl_nat;
#endif
#ifdef HAVE_PBR
    IngressControlPBR() ig_ctl_pbr;
#endif
#ifdef HAVE_INQOS
    IngressControlQosIn() ig_ctl_qos_in;
#endif
#ifdef HAVE_FLOWSPEC
    IngressControlFlowspec() ig_ctl_flowspec;
#endif
#ifdef HAVE_OUTACL
    IngressControlAclOut() ig_ctl_acl_out;
#endif
#ifdef HAVE_OUTQOS
    IngressControlQosOut() ig_ctl_qos_out;
#endif
#ifdef HAVE_MCAST
    IngressControlMcast() ig_ctl_mcast;
#endif
    IngressControlOutPort() ig_ctl_outport;

    Counter< bit<64>, SubIntId_t> ((MAX_PORT+1), CounterType_t.PACKETS_AND_BYTES) pkt_out_stats;

    apply {

        ig_dprsr_md.drop_ctl = 0; // hack for odd/even ports

#ifdef NEED_PKTLEN
        if (hdr.ipv6.isValid()) ig_md.pktlen = hdr.ipv6.payload_len + 40;
#ifdef HAVE_MPLS
        if (hdr.mpls1.isValid()) ig_md.pktlen = ig_md.pktlen + 8;
        else if (hdr.mpls0.isValid()) ig_md.pktlen = ig_md.pktlen + 4;
#endif
#ifdef HAVE_POLKA
        if (hdr.polka.isValid()) ig_md.pktlen = ig_md.pktlen + 20;
#endif
#ifdef HAVE_NSH
        if (hdr.polka.isValid()) ig_md.pktlen = ig_md.pktlen + 8;
#endif
#endif

        ig_ctl_vlan_in.apply(hdr, ig_md, ig_intr_md);

        if (ig_intr_md.ingress_port == CPU_PORT) {
            pkt_out_stats.count(ig_md.source_id);
            ig_tm_md.ucast_egress_port = (PortId_t)hdr.cpu.port;
            ig_tm_md.bypass_egress = 1;
            hdr.cpu.setInvalid();
        } else {
            if (ig_intr_md.ingress_port == RECIR_PORT) {
                hdr.cpu.setInvalid();
            }
#ifdef HAVE_PPPOE
            ig_ctl_pppoe.apply(hdr,ig_md,ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif

#ifdef HAVE_INACL
            ig_ctl_acl_in.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
#ifdef HAVE_INQOS
            ig_ctl_qos_in.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
            ig_ctl_vrf.apply(hdr, ig_md);
#ifdef HAVE_POLKA
            ig_ctl_polka.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
#ifdef HAVE_NSH
            ig_ctl_nsh.apply(hdr,ig_md,ig_intr_md);
#endif
#ifdef HAVE_MPLS
            ig_ctl_mpls.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
#ifdef HAVE_FLOWSPEC
            ig_ctl_flowspec.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
#ifdef HAVE_NAT
            ig_ctl_nat.apply(hdr,ig_md,ig_intr_md);
#endif
#ifdef HAVE_PBR
            ig_ctl_pbr.apply(hdr,ig_md,ig_intr_md);
#endif
#ifdef HAVE_BRIDGE
            ig_ctl_bridge.apply(hdr, ig_md, ig_intr_md);
#endif
            if (ig_md.ipv4_valid == 1) {
                ig_ctl_ipv4.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
            } else if (ig_md.ipv6_valid == 1) {
                ig_ctl_ipv6.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
            } else if (ig_md.arp_valid == 1) {
                ig_md.nexthop_id = CPU_PORT;
            }
#ifdef HAVE_SRV6
            if (ig_md.srv_op_type == 4) {
                ig_ctl_ipv4b.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
            } else if (ig_md.srv_op_type == 6) {
                ig_ctl_ipv6b.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
            }
#endif
            ig_ctl_pkt_pre_emit.apply(hdr, ig_md, ig_intr_md, ig_tm_md);

            if (ig_md.nexthop_id == CPU_PORT) {
#ifdef HAVE_TUN
                ig_ctl_tunnel.apply(hdr,ig_md,ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
#ifdef HAVE_MCAST
                ig_ctl_mcast.apply(hdr,ig_md,ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
#ifdef HAVE_COPP
                ig_ctl_copp.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
            } else {
                if (hdr.vlan.isValid()) hdr.vlan.setInvalid();
#ifdef HAVE_PPPOE
                if (hdr.pppoeD.isValid()) hdr.pppoeD.setInvalid();
#endif
                hdr.ethernet.ethertype = ig_md.ethertype;
                ig_ctl_outport.apply(hdr, ig_md, ig_dprsr_md, ig_tm_md);
#ifdef HAVE_OUTACL
                ig_ctl_acl_out.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
#ifdef HAVE_OUTQOS
                ig_ctl_qos_out.apply(hdr, ig_md, ig_intr_md, ig_dprsr_md, ig_tm_md);
#endif
                ig_ctl_bundle.apply(hdr, ig_md, ig_dprsr_md, ig_tm_md);
            }
        }
    }

#endif

}



#endif // _INGRESS_CONTROL_P4_
