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

#ifndef _IG_CTL_IPv4c_P4_
#define _IG_CTL_IPv4c_P4_

control IngressControlIPv4c(inout headers hdr,
                            inout ingress_metadata_t ig_md,
                            inout standard_metadata_t ig_intr_md) {


    direct_counter(CounterType.packets_and_bytes) statsI;
    direct_counter(CounterType.packets_and_bytes) statsR;
    direct_counter(CounterType.packets_and_bytes) statsN;

    bit<2> mode;

    NextHopId_t neigh;

    SubIntId_t port;

    action act_ipv4_cpl_set_mode(bit<2> mod) {
        mode = mod;
    }


    action act_set_port_nexthop(SubIntId_t subif) {
        port = subif;
    }

    action act_ipv4_cpl_found(NextHopId_t hop) {
        neigh = hop;
    }

    action act_ipv4_cpl_ours(SubIntId_t subif) {
        port = subif;
    }



    action act_set_drop() {
        ig_md.dropping = 1;
        mark_to_drop(ig_intr_md);
    }


    table tbl_port_verify {
        key = {
ig_md.source_id:
            exact;
        }
        actions = {
            act_ipv4_cpl_set_mode;
            @defaultonly NoAction;
        }
        size = VLAN_TABLE_SIZE;
        default_action = NoAction;
        counters = statsI;
    }



    table tbl_ipv4_fib_lpm {
        key = {
hdr.ipv4.src_addr:
            lpm;
ig_md.vrf:
            exact;
        }
        actions = {
            act_ipv4_cpl_found;
            act_ipv4_cpl_ours;
            act_set_drop;
        }
        size = IPV4_LPM_TABLE_SIZE;
        default_action = act_set_drop();
        counters = statsR;
    }



    table tbl_nexthop {
        key = {
neigh:
            exact;
        }
        actions = {
            act_set_port_nexthop;
            act_set_drop;
        }
        size = NEXTHOP_TABLE_SIZE;
        default_action = act_set_drop();
        counters = statsN;
    }


    apply {

        if (ig_md.ipv4_valid!=1) return;

        mode = 0;
        tbl_port_verify.apply();
        if (mode==0) return;

        port = 0;
        neigh = 0;

        tbl_ipv4_fib_lpm.apply();
        if ((neigh==0)&&(port==0)) act_set_drop();

        if (port==0) {
            tbl_nexthop.apply();
            if (mode==2) return;
        }

        if (ig_md.source_id != port) act_set_drop();

    }
}


#endif // _IG_CTL_IPv4c_P4_

