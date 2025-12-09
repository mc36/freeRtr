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

#ifndef _IG_CTL_Rate_out_P4_
#define _IG_CTL_Rate_out_P4_

#ifdef HAVE_OUTRATE

control IngressControlRateOut(inout headers hdr, inout ingress_metadata_t ig_md,
                              in ingress_intrinsic_metadata_t ig_intr_md,
                              inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                              inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    Meter<SubIntId_t>((MAX_PORT+1), MeterType_t.BYTES) rater;


    action act_rate(SubIntId_t metid) {
        ig_md.outrate_res = rater.execute(metid);
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
    }

    apply {
        tbl_rate.apply();
        if (ig_md.outrate_res != MeterColor_t.GREEN) ig_dprsr_md.drop_ctl = 1;
    }
}

#endif

#endif // _IG_CTL_Rate_out_P4_

