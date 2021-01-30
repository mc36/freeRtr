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

#ifndef _EG_CTL_Qos_out_P4_
#define _EG_CTL_Qos_out_P4_

control EgressControlQosOut(inout headers hdr,
                             inout ingress_metadata_t eg_md,
                             inout standard_metadata_t eg_intr_md) {

    meter((MAX_PORT+1), MeterType.bytes) policer;

    action act_deny(SubIntId_t metid) {
        eg_md.meter_id = metid;
        eg_md.dropping = 0;
    }

    action act_permit(SubIntId_t metid) {
        eg_md.meter_id = metid;
        policer.execute_meter((bit<32>)metid, eg_md.meter_res);
        if (eg_md.meter_res == 0) {
            eg_md.dropping = 0;
        } else {
            eg_md.dropping = 1;
        }
    }


    table tbl_ipv4_qos {
        key = {
eg_md.aclport_id:
            exact;
hdr.ipv4.protocol:
            ternary;
hdr.ipv4.src_addr:
            ternary;
hdr.ipv4.dst_addr:
            ternary;
eg_md.layer4_srcprt:
            ternary;
eg_md.layer4_dstprt:
            ternary;
        }
        actions = {
            act_permit;
            act_deny;
            @defaultonly NoAction;
        }
        size = IPV4_OUTQOS_TABLE_SIZE;
        const default_action = NoAction();
    }

    table tbl_ipv6_qos {
        key = {
eg_md.aclport_id:
            exact;
hdr.ipv6.next_hdr:
            ternary;
hdr.ipv6.src_addr:
            ternary;
hdr.ipv6.dst_addr:
            ternary;
eg_md.layer4_srcprt:
            ternary;
eg_md.layer4_dstprt:
            ternary;
        }
        actions = {
            act_permit;
            act_deny;
            @defaultonly NoAction;
        }
        size = IPV6_OUTQOS_TABLE_SIZE;
        const default_action = NoAction();
    }

    apply {
        if (eg_md.ipv4_valid==1)  {
            tbl_ipv4_qos.apply();
        }
        if (eg_md.ipv6_valid==1)  {
            tbl_ipv6_qos.apply();
        }
    }
}

#endif // _EG_CTL_Qos_out_P4_

