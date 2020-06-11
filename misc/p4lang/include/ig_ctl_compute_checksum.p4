#ifndef _IG_CTL_COMPUTE_CHECKSUM_P4_  
#define _IG_CTL_COMPUTE_CHECKSUM_P4_

control ig_ctl_compute_checksum(inout headers hdr, inout ingress_metadata_t ig_md) {
   apply {

      update_checksum(
         hdr.ipv4.isValid(),
            { hdr.ipv4.version,
              hdr.ipv4.ihl,
              hdr.ipv4.diffserv,
              hdr.ipv4.total_len,
              hdr.ipv4.identification,
              hdr.ipv4.flags,
              hdr.ipv4.frag_offset,
              hdr.ipv4.ttl,
              hdr.ipv4.protocol,
              hdr.ipv4.src_addr,
              hdr.ipv4.dst_addr },
              hdr.ipv4.hdr_checksum,
              HashAlgorithm.csum16);

        update_checksum_with_payload(hdr.ipv4.isValid() && hdr.tcp.isValid(), 
            { hdr.ipv4.src_addr, 
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
              hdr.tcp.urgent_ptr },
              hdr.tcp.checksum,
              HashAlgorithm.csum16);

        update_checksum_with_payload(hdr.ipv4.isValid() && hdr.udp.isValid(), 
            { hdr.ipv4.src_addr, 
              hdr.ipv4.dst_addr, 
              8w0, hdr.ipv4.protocol,
              ig_md.layer4_length,
              hdr.udp.src_port,
              hdr.udp.dst_port,
              hdr.udp.length },
              hdr.udp.checksum,
              HashAlgorithm.csum16);

        update_checksum_with_payload(hdr.ipv6.isValid() && hdr.tcp.isValid(), 
            { hdr.ipv6.src_addr, 
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
              hdr.tcp.urgent_ptr },
              hdr.tcp.checksum,
              HashAlgorithm.csum16);

        update_checksum_with_payload(hdr.ipv6.isValid() && hdr.udp.isValid(), 
            { hdr.ipv6.src_addr, 
              hdr.ipv6.dst_addr, 
              8w0, hdr.ipv6.next_hdr,
              ig_md.layer4_length,
              hdr.udp.src_port,
              hdr.udp.dst_port,
              hdr.udp.length },
              hdr.udp.checksum,
              HashAlgorithm.csum16);



   }
}

#endif // _IG_CTL_COMPUTE_CHECKSUM_P4_
