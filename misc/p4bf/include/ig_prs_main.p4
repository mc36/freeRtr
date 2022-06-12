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

#ifndef _INGRESS_PARSER_P4_
#define _INGRESS_PARSER_P4_

parser ig_prs_main(packet_in pkt,
                   /* User */
                   out headers hdr, out ingress_metadata_t ig_md,
                   /* Intrinsic */
                   out ingress_intrinsic_metadata_t ig_intr_md)
{

#include "include/ig_prs_def.p4"

    state start {
        pkt.extract(ig_intr_md);
        //pkt.advance(PORT_METADATA_SIZE);
        ig_md.port_md = port_metadata_unpack<port_metadata_t>(pkt);

#include "include/ig_prs_clr.p4"

        transition meta_init1;
    }

    state meta_init1 {
        transition select(ig_intr_md.ingress_port) {
CPU_PORT:
            prs_cpu;
RECIR_PORT:
            prs_recir;
        default:
            meta_init2;
        }
    }


    state meta_init2 {
        ig_md.ingress_id = ig_md.port_md.portid;
        transition select(ig_intr_md.resubmit_flag) {
1w1:
            prs_resub;
        default:
            prs_ethernet;
        }
    }


    state prs_resub {
        pkt.extract(hdr.internal);
        transition prs_ethernet;
    }


    state prs_cpu {
        ig_md.ingress_id = 0;
        pkt.extract(hdr.cpu);
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ethertype) {
ETHERTYPE_VLAN:
            prs_cpu_vlan;
        default:
            accept;
        }
    }

    state prs_cpu_vlan {
        pkt.extract(hdr.vlan);
        transition accept;
    }

    state prs_recir {
        pkt.extract(hdr.cpu);
        ig_md.ingress_id = hdr.cpu.port;
        transition prs_ethernet;
    }

#include "include/ig_prs_hdr.p4"

}
#endif	// _INGRESS_PARSER_P4_
