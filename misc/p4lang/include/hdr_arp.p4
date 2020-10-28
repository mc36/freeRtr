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

#ifndef _ARP_P4_
#define _ARP_P4_

/*
 * ICMP header: as a header type, order matters
 */
header arp_t {
    bit<16> hrd;
    bit<16> pro;
    bit<8> hln;
    bit<8> pln;
    bit<16> op;
    mac_addr_t sha;
    ipv4_addr_t tpa;
    mac_addr_t tha;
    ipv4_addr_t spa;
}

#endif // _ARP_P4_
