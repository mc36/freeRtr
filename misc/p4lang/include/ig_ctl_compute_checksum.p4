#ifndef _IG_CTL_COMPUTE_CHECKSUM_P4_
#define _IG_CTL_COMPUTE_CHECKSUM_P4_

control ig_ctl_compute_checksum(inout headers hdr, inout ingress_metadata_t ig_md) {
    apply {

        update_checksum(
            (ig_md.punting == 0) && hdr.ipv4.isValid(),
        {   hdr.ipv4.version,
            hdr.ipv4.ihl,
            hdr.ipv4.diffserv,
            hdr.ipv4.total_len,
            hdr.ipv4.identification,
            hdr.ipv4.flags,
            hdr.ipv4.frag_offset,
            hdr.ipv4.ttl,
            hdr.ipv4.protocol,
            hdr.ipv4.src_addr,
            hdr.ipv4.dst_addr
        },
        hdr.ipv4.hdr_checksum,
        HashAlgorithm.csum16);

        update_checksum(
            (ig_md.punting == 0) && hdr.ipv4b.isValid(),
        {   hdr.ipv4b.version,
            hdr.ipv4b.ihl,
            hdr.ipv4b.diffserv,
            hdr.ipv4b.total_len,
            hdr.ipv4b.identification,
            hdr.ipv4b.flags,
            hdr.ipv4b.frag_offset,
            hdr.ipv4b.ttl,
            hdr.ipv4b.protocol,
            hdr.ipv4b.src_addr,
            hdr.ipv4b.dst_addr
        },
        hdr.ipv4b.hdr_checksum,
        HashAlgorithm.csum16);

        update_checksum(
            (ig_md.punting == 0) && hdr.ipv4c.isValid(),
        {   hdr.ipv4c.version,
            hdr.ipv4c.ihl,
            hdr.ipv4c.diffserv,
            hdr.ipv4c.total_len,
            hdr.ipv4c.identification,
            hdr.ipv4c.flags,
            hdr.ipv4c.frag_offset,
            hdr.ipv4c.ttl,
            hdr.ipv4c.protocol,
            hdr.ipv4c.src_addr,
            hdr.ipv4c.dst_addr
        },
        hdr.ipv4c.hdr_checksum,
        HashAlgorithm.csum16);

        update_checksum(
            (ig_md.punting == 0) && hdr.ipv4d.isValid(),
        {   hdr.ipv4d.version,
            hdr.ipv4d.ihl,
            hdr.ipv4d.diffserv,
            hdr.ipv4d.total_len,
            hdr.ipv4d.identification,
            hdr.ipv4d.flags,
            hdr.ipv4d.frag_offset,
            hdr.ipv4d.ttl,
            hdr.ipv4d.protocol,
            hdr.ipv4d.src_addr,
            hdr.ipv4d.dst_addr
        },
        hdr.ipv4d.hdr_checksum,
        HashAlgorithm.csum16);

        update_checksum_with_payload(
            (ig_md.natted == 1) && hdr.ipv4.isValid() && hdr.tcp.isValid(),
        {   hdr.ipv4.src_addr,
            hdr.ipv4.dst_addr,
            8w0, hdr.ipv4.protocol,
            ig_md.layer4_length,
            hdr.tcp.src_port,
            hdr.tcp.dst_port,
            hdr.tcp.seq_no,
            hdr.tcp.ack_no,
            hdr.tcp.data_offset, hdr.tcp.res,
            hdr.tcp.flags,
            hdr.tcp.window,
            hdr.tcp.urgent_ptr
        },
        hdr.tcp.checksum,
        HashAlgorithm.csum16);

        update_checksum_with_payload(
            (ig_md.natted == 1) && hdr.ipv4.isValid() && hdr.udp.isValid(),
        {   hdr.ipv4.src_addr,
            hdr.ipv4.dst_addr,
            8w0, hdr.ipv4.protocol,
            ig_md.layer4_length,
            hdr.udp.src_port,
            hdr.udp.dst_port,
            hdr.udp.length
        },
        hdr.udp.checksum,
        HashAlgorithm.csum16);

        update_checksum_with_payload(
            (ig_md.natted == 1) && hdr.ipv6.isValid() && hdr.tcp.isValid(),
        {   hdr.ipv6.src_addr,
            hdr.ipv6.dst_addr,
            8w0, hdr.ipv6.next_hdr,
            ig_md.layer4_length,
            hdr.tcp.src_port,
            hdr.tcp.dst_port,
            hdr.tcp.seq_no,
            hdr.tcp.ack_no,
            hdr.tcp.data_offset, hdr.tcp.res,
            hdr.tcp.flags,
            hdr.tcp.window,
            hdr.tcp.urgent_ptr
        },
        hdr.tcp.checksum,
        HashAlgorithm.csum16);

        update_checksum_with_payload(
            (ig_md.natted == 1) && hdr.ipv6.isValid() && hdr.udp.isValid(),
        {   hdr.ipv6.src_addr,
            hdr.ipv6.dst_addr,
            8w0, hdr.ipv6.next_hdr,
            ig_md.layer4_length,
            hdr.udp.src_port,
            hdr.udp.dst_port,
            hdr.udp.length
        },
        hdr.udp.checksum,
        HashAlgorithm.csum16);

        /*
                update_checksum_with_payload(
                    (ig_md.punting == 0) && hdr.ipv4d.isValid() && hdr.udp2.isValid(),
                {   hdr.ipv4d.src_addr,
                    hdr.ipv4d.dst_addr,
                    8w0, hdr.ipv4d.protocol,
                    ig_md.layer4_length,
                    hdr.udp2.src_port,
                    hdr.udp2.dst_port,
                    hdr.udp2.length
                },
                hdr.udp2.checksum,
                HashAlgorithm.csum16);

                update_checksum_with_payload(
                    (ig_md.punting == 0) && hdr.ipv6d.isValid() && hdr.udp2.isValid(),
                {   hdr.ipv6d.src_addr,
                    hdr.ipv6d.dst_addr,
                    8w0, hdr.ipv6d.next_hdr,
                    ig_md.layer4_length,
                    hdr.udp2.src_port,
                    hdr.udp2.dst_port,
                    hdr.udp2.length
                },
                hdr.udp2.checksum,
                HashAlgorithm.csum16);
        */

    }
}

#endif // _IG_CTL_COMPUTE_CHECKSUM_P4_
