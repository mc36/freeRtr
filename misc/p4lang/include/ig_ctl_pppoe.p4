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

#ifndef _IG_CTL_PPPOE_P4_
#define _IG_CTL_PPPOE_P4_


control IngressControlPPPOE(inout headers hdr,
                            inout ingress_metadata_t ig_md,
                            inout standard_metadata_t ig_intr_md) {

    action send_to_cpu() {
        /*
         * Prepend cpu header to pkt sent to controller
         * by calling setValid() so it is tekne into account by deparser
         */
        ig_md.nexthop_id = CPU_PORT;
    }


    action act_drop() {
        mark_to_drop(ig_intr_md);
    }

    action act_pppoe_data(SubIntId_t port) {
        ig_md.source_id = port;
        if (hdr.pppoeD.ppptyp == PPPTYPE_SGT) ig_md.ethertype = ETHERTYPE_SGT;
        if (hdr.pppoeD.ppptyp == PPPTYPE_IPV4) ig_md.ethertype = ETHERTYPE_IPV4;
        if (hdr.pppoeD.ppptyp == PPPTYPE_IPV6) ig_md.ethertype = ETHERTYPE_IPV6;
        if (hdr.pppoeD.ppptyp == PPPTYPE_MPLS_UCAST) ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
        if (hdr.pppoeD.ppptyp == PPPTYPE_ROUTEDMAC) ig_md.ethertype = ETHERTYPE_ROUTEDMAC;
    }



    table tbl_pppoe {
        key = {
ig_md.source_id:
            exact;
hdr.pppoeD.session:
            exact;
        }
        actions = {
            act_drop;
            act_pppoe_data;
        }
        size = PPPOE_TABLE_SIZE;
        default_action = act_drop();
    }



    apply {
        /*
         * It is a dataplane packet
         */
        if (ig_md.pppoe_ctrl_valid==1)  {
            send_to_cpu();
            return;
        }
        if (ig_md.pppoe_data_valid!=1) return;

        tbl_pppoe.apply();

        if (ig_md.ethertype != ETHERTYPE_ROUTEDMAC) return;

        hdr.eth5.setValid();
        hdr.eth5 = hdr.ethernet;

    }


}


#endif // _IG_CTL_PPPOE_P4_

