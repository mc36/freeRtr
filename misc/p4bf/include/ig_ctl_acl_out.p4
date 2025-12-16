/*
 * Copyright 2019-present GEANT RARE project
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


#ifndef _IG_CTL_Acl_out_P4_
#define _IG_CTL_Acl_out_P4_

#ifdef HAVE_OUTACL

control IngressControlAclOut(inout headers hdr, inout ingress_metadata_t ig_md,
                             in ingress_intrinsic_metadata_t ig_intr_md,
                             inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                             inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    DirectCounter< bit<64> > (CounterType_t.PACKETS_AND_BYTES) acl4;
    DirectCounter< bit<64> > (CounterType_t.PACKETS_AND_BYTES) acl6;

#ifdef HAVE_INSPECT
    DirectCounter< bit<64> > (CounterType_t.PACKETS_AND_BYTES) insp4;
    DirectCounter< bit<64> > (CounterType_t.PACKETS_AND_BYTES) insp6;
#endif


    action act_deny4() {
        acl4.count();
        ig_dprsr_md.drop_ctl = 1;
    }

    action act_permit4() {
        acl4.count();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }

    action act_deny6() {
        acl6.count();
        ig_dprsr_md.drop_ctl = 1;
    }

    action act_permit6() {
        acl6.count();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }

#ifdef HAVE_RACL
    action act_punt4() {
        acl4.count();
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.nexthop_id = CPU_PORT;
    }

    action act_punt6() {
        acl6.count();
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.nexthop_id = CPU_PORT;
    }
#endif



#ifdef HAVE_INSPECT
    action act_insp4() {
        insp4.count();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }

    action act_insp6() {
        insp6.count();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif


    table tbl_ipv4_acl {
        key = {
ig_md.aclport_id:
            exact;
hdr.ipv4.protocol:
            ternary;
hdr.ipv4.src_addr:
            ternary;
hdr.ipv4.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
hdr.ipv4.diffserv:
            ternary;
hdr.ipv4.identification:
            ternary;
#ifdef HAVE_SGT
ig_md.sec_grp_id:
            ternary;
#endif
        }
        actions = {
            act_permit4;
            act_deny4;
#ifdef HAVE_RACL
            act_punt4;
#endif
            @defaultonly NoAction;
        }
        size = IPV4_OUTACL_TABLE_SIZE;
        const default_action = NoAction();
        counters = acl4;
    }

    table tbl_ipv6_acl {
        key = {
ig_md.aclport_id:
            exact;
hdr.ipv6.next_hdr:
            ternary;
hdr.ipv6.src_addr:
            ternary;
hdr.ipv6.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
hdr.ipv6.traffic_class:
            ternary;
hdr.ipv6.flow_label:
            ternary;
#ifdef HAVE_SGT
ig_md.sec_grp_id:
            ternary;
#endif
        }
        actions = {
            act_permit6;
            act_deny6;
#ifdef HAVE_RACL
            act_punt6;
#endif
            @defaultonly NoAction;
        }
        size = IPV6_OUTACL_TABLE_SIZE;
        const default_action = NoAction();
        counters = acl6;
    }




#ifdef HAVE_INSPECT
    table tbl_ipv4_insp {
        key = {
ig_md.aclport_id:
            exact;
hdr.ipv4.protocol:
            exact;
hdr.ipv4.src_addr:
            exact;
hdr.ipv4.dst_addr:
            exact;
ig_md.layer4_srcprt:
            exact;
ig_md.layer4_dstprt:
            exact;
        }
        actions = {
            act_insp4;
            @defaultonly NoAction;
        }
        size = IPV4_OUTINSP_TABLE_SIZE;
        const default_action = NoAction();
        counters = insp4;
    }

    table tbl_ipv6_insp {
        key = {
ig_md.aclport_id:
            exact;
hdr.ipv6.next_hdr:
            exact;
hdr.ipv6.src_addr:
            exact;
hdr.ipv6.dst_addr:
            exact;
ig_md.layer4_srcprt:
            exact;
ig_md.layer4_dstprt:
            exact;
        }
        actions = {
            act_insp6;
            @defaultonly NoAction;
        }
        size = IPV6_OUTINSP_TABLE_SIZE;
        const default_action = NoAction();
        counters = insp6;
    }
#endif



    apply {
        if (ig_md.ipv4_valid==1)  {
#ifdef HAVE_INSPECT
            if (!tbl_ipv4_insp.apply().hit)
#endif
                tbl_ipv4_acl.apply();
        } else if (ig_md.ipv6_valid==1)  {
#ifdef HAVE_INSPECT
            if (!tbl_ipv6_insp.apply().hit)
#endif
                tbl_ipv6_acl.apply();
        }
    }
}

#endif

#endif // _IG_CTL_Acl_out_P4_
