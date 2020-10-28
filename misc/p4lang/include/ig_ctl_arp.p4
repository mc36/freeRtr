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

#ifndef _IG_CTL_ARP_P4_
#define _IG_CTL_ARP_P4_

control IngressControlARP(inout headers hdr,
                          inout ingress_metadata_t ig_md,
                          inout standard_metadata_t ig_intr_md) {

    action send_to_cpu() {
        /*
         * Prepend cpu header to pkt sent to controller
         * by calling setValid() so it is tekne into account by deparser
         */
        ig_md.nexthop_id = CPU_PORT;
    }

    apply {
        /*
         * It is a dataplane packet
         */
        if (ig_md.arp_valid==1)  {
            send_to_cpu();
        }
    }

}

#endif // _IG_CTL_ARP_P4_
