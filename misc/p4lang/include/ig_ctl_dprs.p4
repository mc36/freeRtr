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

#ifndef _INGRESS_DEPARSER_P4_
#define _INGRESS_DEPARSER_P4_

/*------------------ I N G R E S S  D E P A R S E R ------------------------- */

control ig_ctl_dprs(packet_out pkt,
                    in headers hdr) {
    apply {
        /*
         * parsed headers that have been modified
         * in ctl_ingress and ctl_egress
         * have to be added again into the pkt.
         * for emission in the wire
         */
        pkt.emit(hdr);
    }
}

#endif // _INGRESS_DEPARSER_P4_
