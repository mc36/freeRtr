/*
 * Copyright 2019-present GÉANT RARE project
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


#ifdef CURRSTAGE

//#ifndef _EG_CTL_VLAN_OUT_P4_
//#define _EG_CTL_VLAN_OUT_P4_

control EggressControlVlanOut(inout egress_headers_t hdr, inout egress_metadata_t ig_md,
                              in egress_intrinsic_metadata_t eg_intr_md,
                              inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md)
{

#else

//#ifndef _IG_CTL_VLAN_OUT_P4_
//#define _IG_CTL_VLAN_OUT_P4_

control IngressControlVlanOut(inout ingress_headers hdr, inout ingress_metadata_t ig_md,
                              inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

#endif // CURRSTAGE



    Counter< bit<64>, SubIntId_t> ((MAX_PORT+1), CounterType_t.PACKETS_AND_BYTES) stats;


    action act_set_vlan_port(SubIntId_t port, vlan_id_t vlan) {
        ig_md.output_id = port;
        hdr.vlan.setValid();
        hdr.vlan.ethertype = ig_md.ethertype;
        hdr.ethernet.ethertype = ETHERTYPE_VLAN;
        hdr.vlan.vid = vlan;
    }

    action act_set_port() {
        ig_md.output_id = ig_md.target_id;
        hdr.ethernet.ethertype = ig_md.ethertype;
    }

    table tbl_vlan_out {
        key = {
ig_md.target_id:
            exact;
        }
        actions = {
            act_set_vlan_port;
            @defaultonly act_set_port;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_set_port();
    }

    apply {
        stats.count(ig_md.target_id);
        tbl_vlan_out.apply();
    }
}

//#endif // _IG_CTL_VLAN_OUT_P4_  _EG_CTL_VLAN_OUT_P4_
