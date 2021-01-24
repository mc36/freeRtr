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

#ifndef _EGRESS_PARSER_P4_
#define _EGRESS_PARSER_P4_

parser eg_prs_main(packet_in pkt,
                   /* User */
                   out egress_headers_t hdr, out egress_metadata_t eg_md,
                   /* Intrinsic */
                   out egress_intrinsic_metadata_t eg_intr_md)
{

    state start {
        pkt.extract(eg_intr_md);
        eg_md.target_id = 0;
        eg_md.output_id = 0;
#ifdef HAVE_MPLS
        eg_md.mpls0_valid = 0;
        eg_md.mpls1_valid = 0;
#endif
        eg_md.ipv4_valid = 0;
        eg_md.ipv6_valid = 0;
        pkt.extract(hdr.internal);
        transition prs_ethernet;
    }

    state prs_ethernet {
        pkt.extract(hdr.ethernet);
        eg_md.ethertype = hdr.ethernet.ethertype;
        transition select(hdr.ethernet.ethertype) {
ETHERTYPE_VLAN:
            prs_vlan;
#ifdef HAVE_MPLS
ETHERTYPE_MPLS_UCAST:
            prs_mpls0;
#endif
ETHERTYPE_IPV4:
            prs_ipv4;
ETHERTYPE_IPV6:
            prs_ipv6;
        default:
            accept;
        }
    }

    state prs_vlan {
        pkt.extract(hdr.vlan);
        eg_md.ethertype = hdr.vlan.ethertype;
        transition select(hdr.vlan.ethertype) {
#ifdef HAVE_MPLS
ETHERTYPE_MPLS_UCAST:
            prs_mpls0;
#endif
ETHERTYPE_IPV4:
            prs_ipv4;
ETHERTYPE_IPV6:
            prs_ipv6;
        default:
            accept;
        }
    }

#ifdef HAVE_MPLS
    state prs_mpls0 {
        pkt.extract(hdr.mpls0);
        eg_md.mpls0_valid = 1;
        transition select(hdr.mpls0.bos) {
0:
            prs_mpls1;
1:
            prs_mpls_bos;
        default:
            accept;
        }
    }
#endif

#ifdef HAVE_MPLS
    state prs_mpls1 {
        pkt.extract(hdr.mpls1);
        eg_md.mpls1_valid = 1;
        transition select(hdr.mpls1.bos) {
1w0:
            accept;
1w1:
            prs_mpls_bos;
        default:
            accept;
        }
    }
#endif

#ifdef HAVE_MPLS
    state prs_mpls_bos {
        transition select((pkt.lookahead < bit < 4 >> ())[3:0]) {
0x4:
            prs_ipv4;		/* IPv4 only for now */
0x6:
            prs_ipv6;		/* IPv6 is in next lab */
        default:
            accept;		/* EoMPLS is pausing problem if we don't resubmit() */
        }
    }
#endif

    state prs_ipv4 {
        pkt.extract(hdr.ipv4);
        eg_md.ipv4_valid = 1;
        transition accept;
    }

    state prs_ipv6 {
        pkt.extract(hdr.ipv6);
        eg_md.ipv6_valid = 1;
        transition accept;
    }

}

#endif	// _EGRESS_PARSER_P4_
