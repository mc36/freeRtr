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

#ifndef _IG_CTL_rate_out_P4_
#define _IG_CTL_rate_out_P4_

control IngressControlRateOut(inout headers hdr,
                              inout ingress_metadata_t ig_md,
                              inout standard_metadata_t ig_intr_md) {

    meter((MAX_PORT+1), MeterType.bytes) rate;
    direct_counter(CounterType.packets_and_bytes) statsr;


    action act_rate(SubIntId_t metid) {
        ig_md.meter_id = metid;
        rate.execute_meter((bit<32>)metid, ig_md.meter_res);
        if (ig_md.meter_res != 0) {
            ig_md.dropping = 1;
        }
    }



    table tbl_rate {
        key = {
ig_md.aclport_id:
            exact;
        }
        actions = {
            act_rate;
            @defaultonly NoAction;
        }
        size = MAX_PORT;
        const default_action = NoAction();
        counters = statsr;
    }

    apply {
        tbl_rate.apply();
    }
}

#endif // _IG_CTL_rate_out_P4_

