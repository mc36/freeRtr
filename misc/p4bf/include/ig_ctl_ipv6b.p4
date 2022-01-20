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

#ifndef _IG_CTL_IPv6b_P4_
#define _IG_CTL_IPv6b_P4_

#ifdef HAVE_SRV6

control IngressControlIPv6b(inout headers hdr, inout ingress_metadata_t ig_md,
                            in ingress_intrinsic_metadata_t ig_intr_md,
                            inout ingress_intrinsic_metadata_for_deparser_t
                            ig_dprsr_md,
                            inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    action act_ipv6_cpl_set_nexthop() {
        ig_md.nexthop_id = CPU_PORT;
    }

    action act_ipv6_fib_discard() {
        ig_dprsr_md.drop_ctl = 1;
    }

    action act_ipv6_set_nexthop(NextHopId_t nexthop_id) {
        ig_md.nexthop_id = nexthop_id;
    }

#ifdef HAVE_POLKA
    action act_ipv6_polka_encap_set_nexthop(polka_route_t routeid,
                                            NextHopId_t nexthop_id) {
        hdr.polka.setValid();
        hdr.polka.version = 0;
        hdr.polka.ttl = hdr.ipv6.hop_limit;
        hdr.polka.routeid = routeid;
        hdr.polka.proto = ETHERTYPE_IPV6;
        ig_md.ethertype = ETHERTYPE_POLKA;
        ig_md.nexthop_id = nexthop_id;
        ig_md.polka_remove = 0;
    }
#endif

#ifdef HAVE_MPLS
    action act_ipv6_mpls2_encap_set_nexthop(label_t vpn_label,
                                            label_t egress_label,
                                            NextHopId_t nexthop_id) {
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_encap_egress_label = egress_label;
        ig_md.mpls_encap_svc_label = vpn_label;
        ig_md.mpls_encap_l3vpn_valid = 1;
        ig_md.nexthop_id = nexthop_id;
    }
#endif

#ifdef HAVE_MPLS
    action act_ipv6_mpls1_encap_set_nexthop(label_t egress_label,
                                            NextHopId_t nexthop_id) {
        ig_md.mpls0_remove = 0;
        ig_md.mpls_encap_egress_label = egress_label;
        ig_md.mpls_encap_rawip_valid = 1;
        ig_md.nexthop_id = nexthop_id;
    }
#endif

#ifdef HAVE_SRV6
    action act_ipv6_srv_encap_set_nexthop(ipv6_addr_t target, NextHopId_t nexthop_id) {
        ig_md.srv_target = target;
        ig_md.srv_encap_l3vpn_valid = 1;
        ig_md.nexthop_id = nexthop_id;
    }
#endif

    table tbl_ipv6_fib_host {
        key = {
hdr.ipv6b.dst_addr:
            exact;
ig_md.vrf:
            exact;
        }
        actions = {
            act_ipv6_cpl_set_nexthop;
            act_ipv6_set_nexthop;
#ifdef HAVE_POLKA
            act_ipv6_polka_encap_set_nexthop;
#endif
#ifdef HAVE_MPLS
            act_ipv6_mpls2_encap_set_nexthop;
#endif
#ifdef HAVE_MPLS
            act_ipv6_mpls1_encap_set_nexthop;
#endif
#ifdef HAVE_SRV6
            act_ipv6_srv_encap_set_nexthop;
#endif
            @defaultonly NoAction;
        }
        size = IPV6_HOST_TABLE_SIZE;
        const default_action = NoAction();
    }

#ifdef HAVE_ALPM
    @pragma alpm 1
    @pragma alpm_partitions 4096
#endif
    table tbl_ipv6_fib_lpm {
        key = {
hdr.ipv6b.dst_addr:
            lpm;
ig_md.vrf:
            exact;
        } actions = {
            act_ipv6_cpl_set_nexthop;
            act_ipv6_set_nexthop;
#ifdef HAVE_POLKA
            act_ipv6_polka_encap_set_nexthop;
#endif
#ifdef HAVE_MPLS
            act_ipv6_mpls2_encap_set_nexthop;
#endif
#ifdef HAVE_MPLS
            act_ipv6_mpls1_encap_set_nexthop;
#endif
#ifdef HAVE_SRV6
            act_ipv6_srv_encap_set_nexthop;
#endif
            act_ipv6_fib_discard;
            @defaultonly NoAction;
        }
        size = IPV6_LPM_TABLE_SIZE;
        default_action = NoAction();
    }

    apply {
        ig_md.mpls_encap_decap_sap_type = 6;
        if (hdr.ipv6b.next_hdr==IP_PROTOCOL_HOPOPT) {
            act_ipv6_cpl_set_nexthop();
        } else if (!tbl_ipv6_fib_host.apply().hit) {
            tbl_ipv6_fib_lpm.apply();
        }
    }
}

#endif

#endif // _IG_CTL_IPv6b_P4_
