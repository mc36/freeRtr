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

// source here: 
// https://www.iana.org/assignments/ieee-802-numbers/ieee-802-numbers.xhtml 

#ifndef _ETHERTYPE_P4_
#define _ETHERTYPE_P4_

const bit<16> ETHERTYPE_IPV4              = 0x0800;
const bit<16> ETHERTYPE_ARP               = 0x0806;
const bit<16> ETHERTYPE_VLAN              = 0x8100;
const bit<16> ETHERTYPE_IPV6              = 0x86dd;
const bit<16> ETHERTYPE_MPLS_UCAST        = 0x8847;
const bit<16> ETHERTYPE_MPLS_MCAST        = 0x8848;
const bit<16> ETHERTYPE_LACP              = 0x8809;
const bit<16> ETHERTYPE_LLDP              = 0x88cc;
const bit<16> ETHERTYPE_PPPOE_CTRL        = 0x8863;
const bit<16> ETHERTYPE_PPPOE_DATA        = 0x8864;

#endif // _ETHERTYPE_P4_
