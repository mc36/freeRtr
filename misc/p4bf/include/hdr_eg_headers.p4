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

#ifndef _EGRESS_HEADERS_P4_
#define _EGRESS_HEADERS_P4_

struct egress_headers_t {
    internal_header_t internal;
    ethernet_t ethernet;
    vlan_t vlan;
#ifdef HAVE_MPLS
    mpls_t mpls0;
    mpls_t mpls1;
#endif
    ipv4_t ipv4;
    ipv6_t ipv6;
}

#endif	// _EGRESS_HEADERS_P4_
