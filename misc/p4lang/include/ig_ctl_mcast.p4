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

#ifndef _IG_CTL_MCAST_P4_
#define _IG_CTL_MCAST_P4_


control IngressControlMcast(inout headers hdr,
                            inout ingress_metadata_t ig_md,
                            inout standard_metadata_t ig_intr_md) {


    action act_local(SubIntId_t ingr, bit<16> sess) {
        ig_md.clone_session = sess;
        ig_md.need_clone = 0;
        if (ingr != ig_md.source_id) {
            ig_md.dropping = 1;
            return;
        }
    }

    action act_flood(SubIntId_t ingr, bit<16> sess) {
        ig_md.clone_session = sess;
        if (ingr != ig_md.source_id) {
            ig_md.need_clone = 0;
            ig_md.dropping = 1;
            return;
        }
        ig_md.need_clone = 1;
    }


    table tbl_mcast4 {
        key = {
ig_md.vrf:
            exact;
hdr.ipv4.src_addr:
            exact;
hdr.ipv4.dst_addr:
            exact;
        }
        actions = {
            act_local;
            act_flood;
            @defaultonly NoAction;
        }
        size = IPV4_MCAST_TABLE_SIZE;
        const default_action = NoAction();
    }


    table tbl_mcast6 {
        key = {
ig_md.vrf:
            exact;
hdr.ipv6.src_addr:
            exact;
hdr.ipv6.dst_addr:
            exact;
        }
        actions = {
            act_local;
            act_flood;
            @defaultonly NoAction;
        }
        size = IPV6_MCAST_TABLE_SIZE;
        const default_action = NoAction();
    }


    apply {

        ig_md.need_clone = 0;

        if (hdr.ipv4.isValid()) {
            tbl_mcast4.apply();
        }
        if (hdr.ipv6.isValid()) {
            tbl_mcast6.apply();
        }

        if (ig_md.need_clone == 0) return;

        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        if (hdr.ipv4.isValid()) ig_md.ethertype = ETHERTYPE_IPV4;
        if (hdr.ipv6.isValid()) ig_md.ethertype = ETHERTYPE_IPV6;

        ig_intr_md.mcast_grp = ig_md.clone_session;
//        clone(CloneType.I2E, (bit<32>)ig_md.clone_session);

    }


}


#endif // _IG_CTL_MCAST_P4_
