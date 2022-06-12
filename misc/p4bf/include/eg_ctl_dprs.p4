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

#ifndef _EGRESS_DEPARSER_P4_
#define _EGRESS_DEPARSER_P4_

control eg_ctl_dprs(packet_out pkt,
                    /* User */
                    inout headers hdr, in ingress_metadata_t eg_md,
                    /* Intrinsic */
                    in egress_intrinsic_metadata_for_deparser_t eg_dprsr_md)
{

    Checksum()ipv4_checksum;

    apply {

        hdr.ipv4.hdr_checksum = ipv4_checksum.update( {
            hdr.ipv4.version, hdr.ipv4.ihl,
            hdr.ipv4.diffserv,
            hdr.ipv4.total_len,
            hdr.ipv4.identification,
            hdr.ipv4.flags,
            hdr.ipv4.frag_offset,
            hdr.ipv4.ttl,
            hdr.ipv4.protocol,
            hdr.ipv4.src_addr,
            hdr.ipv4.dst_addr
        }                                                    );

#ifdef HAVE_TUN
        hdr.ipv4d.hdr_checksum = ipv4_checksum.update( {
            hdr.ipv4d.version, hdr.ipv4d.ihl,
            hdr.ipv4d.diffserv,
            hdr.ipv4d.total_len,
            hdr.ipv4d.identification,
            hdr.ipv4d.flags,
            hdr.ipv4d.frag_offset,
            hdr.ipv4d.ttl,
            hdr.ipv4d.protocol,
            hdr.ipv4d.src_addr,
            hdr.ipv4d.dst_addr
        }                                                    );
#endif

        pkt.emit(hdr);

    }
}

#endif // _EGRESS_DEPARSER_P4_
