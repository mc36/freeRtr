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

#ifndef _INGRESS_METADATA_P4_
#define _INGRESS_METADATA_P4_

/*
 * User defined metadata type
 */
struct ingress_metadata_t {
    SubIntId_t ingress_id;
    SubIntId_t source_id;
    SubIntId_t target_id;
    SubIntId_t aclport_id;
    NextHopId_t nexthop_id;
    SubIntId_t outport_id;
    SubIntId_t bridge_id;
    SubIntId_t bridge_src;
    SubIntId_t bridge_trg;
    SubIntId_t meter_id;
    bit<32> meter_res;
    @field_list(1)
    bit<4> hash_id;
    ethertype_t ethertype;
    switch_vrf_t vrf;
    bit<16> polka_next;
    bit<2>  dropping;
    bit<1>  punting;
    bit<1>  natted;
    bit<2>  need_recir;
    bit<1>  need_clone;
    bit<16> clone_session;
    bit<3>  mpls_op_type;
    bit<3>  srv_op_type;
    bit<3>  amt_type;
    bit<3>  gtp_type;
    bit<16> vlan_size;
    bit<1>  sgt_remove;
    bit<1>  polka_remove;
    bit<1>  nsh_remove;
    bit<1>  mpls0_remove;
    bit<1>  mpls1_remove;
    bit<1>  pppoe_ctrl_valid;
    bit<1>  pppoe_data_valid;
    bit<1>  sgt_valid;
    bit<1>  polka_valid;
    bit<1>  nsh_valid;
    bit<1>  mpls0_valid;
    bit<1>  mpls1_valid;
    bit<1>  llc_valid;
    bit<1>  arp_valid;
    bit<1>  ipv4_valid;
    bit<1>  ipv6_valid;
    layer4_port_t  layer4_srcprt;
    layer4_port_t  layer4_dstprt;
    bit<16> sec_grp_id;
    bit<16> layer4_length;
}

#endif // _INGRESS_METADATA_P4_
