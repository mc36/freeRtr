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

#ifndef _IG_CTL_BUNDLE_P4_
#define _IG_CTL_BUNDLE_P4_

control IngressControlBundle(inout headers hdr,
                             inout ingress_metadata_t ig_md,
                             inout standard_metadata_t ig_intr_md) {

    action act_set_hash(PortId_t port) {
        ig_intr_md.egress_spec = port;
        ig_md.vlan_size = 0;
        ig_md.need_recir = 0;
    }

    action act_set_port() {
        ig_intr_md.egress_spec = (PortId_t)ig_md.outport_id;
        ig_md.vlan_size = 0;
        ig_md.need_recir = 0;
    }

    action act_set_recir(SubIntId_t port) {
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.vlan_size = 2;
        ig_md.need_recir = 2;
        hdr.cpu.setValid();
        hdr.cpu.port = port;
    }

    table tbl_bundle {
        key = {
ig_md.outport_id:
            exact;
ig_md.hash_id:
            exact;
        }
        actions = {
            act_set_port;
            act_set_hash;
            act_set_recir;
        }
        size = BUNDLE_TABLE_SIZE;
        default_action = act_set_port();
    }

    apply {
        bit<16> tmp = ig_md.layer4_srcprt ^ ig_md.layer4_dstprt;
        tmp = (tmp >> 8) ^ (tmp & 0xff);
        tmp = (tmp >> 4) ^ (tmp & 0xf);
        ig_md.hash_id = (bit<4>)tmp;
        tbl_bundle.apply();
    }
}

#endif // _IG_CTL_BUNDLE_P4_

