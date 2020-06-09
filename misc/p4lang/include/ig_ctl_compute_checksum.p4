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
   }
}

#endif // _IG_CTL_COMPUTE_CHECKSUM_P4_
