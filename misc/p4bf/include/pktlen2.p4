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


#ifdef NEED_PKTLEN
pktlen = 0;
if (hdr.ipv4.isValid()) pktlen = hdr.ipv4.total_len;
else if (hdr.ipv6.isValid()) pktlen = hdr.ipv6.payload_len + 40;
else if (hdr.arp.isValid()) pktlen = 28;
#ifdef HAVE_MPLS
if (hdr.mpls0.isValid()) pktlen = pktlen + 4;
if (hdr.mpls1.isValid()) pktlen = pktlen + 4;
#endif
#ifdef HAVE_TAP
if (hdr.eth4.isValid()) pktlen = pktlen + 14;
#endif
#endif
