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

#ifndef _IG_CTL_Acl_in_P4_
#define _IG_CTL_Acl_in_P4_

control IngressControlAclIn(inout headers hdr,
                            inout ingress_metadata_t ig_md,
                            inout standard_metadata_t ig_intr_md) {

    direct_counter(CounterType.packets_and_bytes) acl4;
    direct_counter(CounterType.packets_and_bytes) acl6;

    direct_counter(CounterType.packets_and_bytes) insp4;
    direct_counter(CounterType.packets_and_bytes) insp6;

    action act_deny() {
        ig_md.dropping = 1;
    }

    action act_permit() {
        ig_md.dropping = (bit<2>)ig_md.layer3_frag;
    }

    action act_punt() {
        ig_md.dropping = 2;
    }


    table tbl_ipv4_acl {
        key = {
ig_md.source_id:
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
ig_md.sec_grp_id:
            ternary;
        }
        actions = {
            act_permit;
            act_deny;
            act_punt;
            @defaultonly NoAction;
        }
        size = IPV4_INACL_TABLE_SIZE;
        const default_action = NoAction();
        counters = acl4;
    }

    table tbl_ipv6_acl {
        key = {
ig_md.source_id:
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
ig_md.sec_grp_id:
            ternary;
        }
        actions = {
            act_permit;
            act_deny;
            act_punt;
            @defaultonly NoAction;
        }
        size = IPV6_INACL_TABLE_SIZE;
        const default_action = NoAction();
        counters = acl6;
    }



    table tbl_ipv4_insp {
        key = {
ig_md.source_id:
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
            act_permit;
            act_deny;
        }
        size = IPV4_ININSP_TABLE_SIZE;
        const default_action = act_deny();
        counters = insp4;
    }

    table tbl_ipv6_insp {
        key = {
ig_md.source_id:
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
            act_permit;
            act_deny;
        }
        size = IPV6_ININSP_TABLE_SIZE;
        const default_action = act_deny();
        counters = insp6;
    }



    apply {
        if (ig_md.ipv4_valid==1)  {
            if (tbl_ipv4_insp.apply().hit) return;
            ig_md.dropping = 0;
            tbl_ipv4_acl.apply();
        }
        if (ig_md.ipv6_valid==1)  {
            if (tbl_ipv6_insp.apply().hit) return;
            ig_md.dropping = 0;
            tbl_ipv6_acl.apply();
        }
    }
}

#endif // _IG_CTL_Acl_in_P4_

