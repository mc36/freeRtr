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

#ifndef _IG_CTL_Flowspec_P4_
#define _IG_CTL_Flowspec_P4_

control IngressControlFlowspec(inout headers hdr,
                               inout ingress_metadata_t ig_md,
                               inout standard_metadata_t ig_intr_md) {

    meter((IPV4_FLOWSPEC_TABLE_SIZE), MeterType.bytes) policer4;
    meter((IPV6_FLOWSPEC_TABLE_SIZE), MeterType.bytes) policer6;
    direct_counter(CounterType.packets_and_bytes) stats4;
    direct_counter(CounterType.packets_and_bytes) stats6;

    action act4_deny(SubIntId_t metid) {
        ig_md.meter_id = metid;
        ig_md.dropping = 0;
    }

    action act4_permit(SubIntId_t metid) {
        ig_md.meter_id = metid;
        policer4.execute_meter((bit<32>)metid, ig_md.meter_res);
        if (ig_md.meter_res == 0) {
            ig_md.dropping = 0;
        } else {
            ig_md.dropping = 1;
        }
    }

    action act6_deny(SubIntId_t metid) {
        ig_md.meter_id = metid;
        ig_md.dropping = 0;
    }

    action act6_permit(SubIntId_t metid) {
        ig_md.meter_id = metid;
        policer6.execute_meter((bit<32>)metid, ig_md.meter_res);
        if (ig_md.meter_res == 0) {
            ig_md.dropping = 0;
        } else {
            ig_md.dropping = 1;
        }
    }


    table tbl_ipv4_flowspec {
        key = {
ig_md.vrf:
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
            act4_permit;
            act4_deny;
            @defaultonly NoAction;
        }
        size = IPV4_FLOWSPEC_TABLE_SIZE;
        const default_action = NoAction();
        counters = stats4;
    }

    table tbl_ipv6_flowspec {
        key = {
ig_md.vrf:
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
            act6_permit;
            act6_deny;
            @defaultonly NoAction;
        }
        size = IPV6_FLOWSPEC_TABLE_SIZE;
        const default_action = NoAction();
        counters = stats6;
    }

    apply {
        if (ig_md.ipv4_valid==1)  {
            tbl_ipv4_flowspec.apply();
        }
        if (ig_md.ipv6_valid==1)  {
            tbl_ipv6_flowspec.apply();
        }
    }
}

#endif // _IG_CTL_Flowspec_P4_

