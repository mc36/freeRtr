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

#ifndef _EG_CTL_OUTPORT_P4_
#define _EG_CTL_OUTPORT_P4_

control EgressControlOutPort(inout headers hdr,
                             inout ingress_metadata_t eg_md,
                             inout standard_metadata_t eg_intr_md) {


    action act_set_port_nexthop(SubIntId_t subif) {
        eg_md.aclport_id = subif;
    }


    table tbl_nexthop {
        key = {
eg_md.nexthop_id:
            exact;
        }
        actions = {
            act_set_port_nexthop;
            @defaultonly NoAction;
        }
        size = NEXTHOP_TABLE_SIZE;
        default_action = NoAction();
    }



    apply {

        tbl_nexthop.apply();

    }
}

#endif // _EG_CTL_OUTPORT_P4_

