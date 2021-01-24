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

#ifndef _TYPES_P4_
#define _TYPES_P4_

typedef bit<16> ethertype_t;
typedef bit<48> mac_addr_t;
typedef bit<20> label_t;
typedef bit<32> ipv4_addr_t;
typedef bit<128> ipv6_addr_t;
typedef bit<16> layer4_port_t;
typedef bit<12> vlan_id_t;
typedef bit<16> switch_vrf_t;
typedef bit<9> PortId_t;
typedef bit<16> NextHopId_t;
typedef bit<10> SubIntId_t;
#define MAX_PORT 1023

#endif // _TYPES_P4_
