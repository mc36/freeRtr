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


#ifndef _INTERNAL_
#define _INTERNAL_

#define INTREAS_UCAST 1
#define INTREAS_RECIR 2
#define INTREAS_MCAST 3

header internal_header_t {
#ifdef NEED_PKTLEN
    bit<16> pktlen;
#endif
    bit<16> clone_session;
#ifdef HAVE_SGT
    bit<16> sec_grp_id;
#endif
    NextHopId_t nexthop_id;
    bit<6> _padding1;
    SubIntId_t target_id;
    bit<6> _padding2;
    SubIntId_t source_id;
    bit<6> _padding3;
    SubIntId_t aclport_id;
    bit<8> reason;
}

#endif // _INTERNAL_
