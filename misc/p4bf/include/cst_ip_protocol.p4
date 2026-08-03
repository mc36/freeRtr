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

// source here:
// https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml

#ifndef _IP_PROTOCOL_P4_
#define _IP_PROTOCOL_P4_

const bit <8> IP_PROTOCOL_HOPOPT = 0;
const bit <8> IP_PROTOCOL_ICMP = 1;
const bit <8> IP_PROTOCOL_IGMP = 2;
const bit <8> IP_PROTOCOL_IPV4 = 4;
const bit <8> IP_PROTOCOL_ST = 5;
const bit <8> IP_PROTOCOL_TCP = 6;
const bit <8> IP_PROTOCOL_UDP = 17;
const bit <8> IP_PROTOCOL_TMUX = 18;
const bit <8> IP_PROTOCOL_IPV6 = 41;
const bit <8> IP_PROTOCOL_IPV6_ROUTE = 43;
const bit <8> IP_PROTOCOL_IPV6_FRAG = 44;
const bit <8> IP_PROTOCOL_RSVP = 46;
const bit <8> IP_PROTOCOL_GRE = 47;
const bit <8> IP_PROTOCOL_SKIP = 57;
const bit <8> IP_PROTOCOL_IPV6_ICMP = 58;
const bit <8> IP_PROTOCOL_IPV6_NONXT = 59;
const bit <8> IP_PROTOCOL_IPV6_OPTS = 60;
const bit <8> IP_PROTOCOL_EIGRP = 88;
const bit <8> IP_PROTOCOL_OSPFIGP = 89;
const bit <8> IP_PROTOCOL_ETHERIP = 97;
const bit <8> IP_PROTOCOL_PIM = 103;
const bit <8> IP_PROTOCOL_VRRP = 112;
const bit <8> IP_PROTOCOL_L2TP = 115;
const bit <8> IP_PROTOCOL_STP = 118;
const bit <8> IP_PROTOCOL_RSVP_E2E_IGNORE = 134;
const bit <8> IP_PROTOCOL_MOBILITY_HEADER = 135;
const bit <8> IP_PROTOCOL_MPLS_IN_IP = 137;
const bit <8> IP_PROTOCOL_SRL2 = 143;
const bit <8> IP_PROTOCOL_NSH = 145;

#endif // _IP_PROTOCOL_P4_
