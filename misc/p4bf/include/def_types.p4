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


#ifndef _TYPES_P4_
#define _TYPES_P4_

typedef bit <16> ethertype_t;
typedef bit <48> mac_addr_t;
#ifdef HAVE_MPLS
typedef bit <20> label_t;
#endif
typedef bit <32> ipv4_addr_t;
typedef bit <128> ipv6_addr_t;
typedef bit<16> layer4_port_t;
typedef bit <12> vlan_id_t;
typedef bit <16> switch_vrf_t;
//typedef bit<9> PortId_t;
typedef bit<16> NextHopId_t;
typedef bit<9> SubIntId_t;
#define MAX_PORT 1023

struct l4_lookup_t {
    bit<16>  word_1;
    bit<16>  word_2;
}

/*
 * Since we will be calculating hash in 32-bit pieces, we will have this
 * definition, which will be either bit<32>, bit<64> or bit<96> depending
 * on HASH_WIDTH
 */
typedef bit<(((HASH_WIDTH + 32)/32)*32)> selector_hash_t;

#endif // _TYPES_P4_
