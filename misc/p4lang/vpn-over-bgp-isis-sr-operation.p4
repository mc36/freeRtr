/*
 * P4 language version: P4_16 
 */

/*
 * include P4 core library 
 */
#include <core.p4>

/* 
 * include P4 v1model library implemented by simple_switch 
 */
#include <v1model.p4>

/*
 * include Ethertype mapping 
 */
#include "ethertype.p4"

/* 
 * include IP protocol mapping 
 */
#include "ip-protocol.p4"

/* 
 * include P4 table size declaration 
 */
#include "p4-table.p4"

/* 
 * include P4 switch port information 
 */
#include "p4-switch-port.p4"

/*
 * egress_spec port encoded using 9 bits
 */ 
typedef bit<9>  egress_spec_t;

/*
 * HW MAC address encoded using 48 bits
 */
typedef bit<48> mac_addr_t;

/*
 * IPv4 address encoded using 32 bits
 */
typedef bit<32> ipv4_addr_t;

/*
 * Nexthop using 16 bits
 */
typedef bit<9> nexthop_id_t;

/*
 * MPLS label using 20 bits
 */
typedef bit<20> label_t;


/*
 * Ethernet header: as a header type, order matters
 */
header ethernet_t {
   mac_addr_t dst_mac_addr;
   mac_addr_t src_mac_addr;
   bit<16>   ethertype;
}

/*
 * LLC header: as a header type, order matters
 */
header llc_header_t {
   bit<8> dsap;
   bit<8> ssap;
   bit<8> control_;
}

/*
 * MPLS header: as a header type, order matters
 */
header mpls_t {
   bit<20> label;
   bit<3>  exp;
   bit<1>  bos;
   bit<8>  ttl;
}

/*
 * ICMP header: as a header type, order matters
 */
header icmp_t {
    bit<16> type_code;
    bit<16> hdr_checksum;
}

/*
 * IPv4 header: as a header type, order matters
 */
header ipv4_t {
   bit<4>    version;
   bit<4>    ihl;
   bit<8>    diffserv;
   bit<16>   total_len;
   bit<16>   identification;
   bit<3>    flags;
   bit<13>   frag_offset;
   bit<8>    ttl;
   bit<8>    protocol;
   bit<16>   hdr_checksum;
   ipv4_addr_t src_addr;
   ipv4_addr_t dst_addr;
}

/*
 * IPv6 header: as a header type, order matters
 */
header ipv6_t {
    bit<4>   version;
    bit<8>   traffic_class;
    bit<20>  flow_label;
    bit<16>  payload_len;
    bit<8>   next_hdr;
    bit<8>   hop_limit;
    bit<128> src_addr;
    bit<128> dst_addr;
}


header tcp_t {
    bit<16> src_port;
    bit<16> dst_port;
    bit<32> seq_no;
    bit<32> ack_no;
    bit<4>  data_offset;
    bit<4>  res;
    bit<8>  flags;
    bit<16> window;
    bit<16> checksum;
    bit<16> urgent_ptr;
}

header udp_t {
    bit<16> src_port;
    bit<16> dst_port;
    bit<16> length_;
    bit<16> checksum;
}


struct ingress_intrinsic_metadata_t {
    bit<1>  resubmit_flag;
    bit<48> ingress_global_timestamp;
    bit<16> mcast_grp;
    bit<1>  deflection_flag;
    bit<1>  deflect_on_drop;
    bit<2>  enq_congest_stat;
    bit<2>  deq_congest_stat;
    bit<13> mcast_hash;
    bit<16> egress_rid;
    bit<32> lf_field_list;
    bit<3>  priority;
    bit<3>  ingress_cos;
    bit<2>  packet_color;
    bit<5>  qid;
}

/*
 * L3 metadata type  
 */
struct l3_metadata_t {
    bit<2>  lkp_ip_type;
    bit<4>  lkp_ip_version;
    bit<8>  lkp_ip_proto;
    bit<8>  lkp_dscp;
    bit<8>  lkp_ip_ttl;
    bit<16> lkp_l4_sport;
    bit<16> lkp_l4_dport;
    bit<16> lkp_outer_l4_sport;
    bit<16> lkp_outer_l4_dport;
    bit<16> vrf;
    bit<10> rmac_group;
    bit<1>  rmac_hit;
    bit<2>  urpf_mode;
    bit<1>  urpf_hit;
    bit<1>  urpf_check_fail;
    bit<16> urpf_bd_group;
    bit<1>  fib_hit;
    bit<16> fib_nexthop;
    bit<2>  fib_nexthop_type;
    bit<16> same_bd_check;
    bit<16> nexthop_index;
    bit<1>  routed;
    bit<1>  outer_routed;
    bit<8>  mtu_index;
    bit<1>  l3_copy;
    bit<16> l3_mtu_check;
    bit<16> egress_l4_sport;
    bit<16> egress_l4_dport;
}

/*
 * IPv4 metadata type
 */
struct ipv4_metadata_t {
    ipv4_addr_t lkp_ipv4_sa;
    ipv4_addr_t lkp_ipv4_da;
    bit<1>  ipv4_unicast_enabled;
    bit<2>  ipv4_urpf_mode;
}

/*
 * IPv6 metadata type
 */
struct ipv6_metadata_t {
    bit<128> lkp_ipv6_sa;
    bit<128> lkp_ipv6_da;
    bit<1>   ipv6_unicast_enabled;
    bit<1>   ipv6_src_is_link_local;
    bit<2>   ipv6_urpf_mode;
}

/*
 * metadata type  
 */
struct metadata_t {
   nexthop_id_t                 nexthop_id;
   ingress_intrinsic_metadata_t intrinsic_metadata;
   l3_metadata_t                l3_metadata;
   ipv4_metadata_t              ipv4_metadata;
   ipv6_metadata_t              ipv6_metadata;
}

/*
 * Our P4 program header structure 
 */
struct headers {
   ethernet_t   ethernet;
   ipv4_t       ipv4;
   ipv6_t       ipv6;
   llc_header_t llc_header;
   tcp_t        tcp;
   udp_t        udp;
   mpls_t[3]    mpls;
}

/*
 * V1Model PARSER
 */
parser prs_main(packet_in packet,
                out headers hdr,
                inout metadata_t md,
                inout standard_metadata_t std_md) {

   state start {
      packet.extract(hdr.ethernet);
      md.intrinsic_metadata.priority = 0;
      transition select(hdr.ethernet.ethertype) {
         0 &&& 0xfe00: prs_llc_header; /* LLC SAP frame */
         0 &&& 0xfa00: prs_llc_header; /* LLC SAP frame */
         ETHERTYPE_MPLS_UCAST : prs_mpls;
         ETHERTYPE_IPV4: prs_ipv4;
         default: accept;
      }
   }

   state prs_mpls {
      packet.extract(hdr.mpls.next);
      transition select(hdr.mpls.last.bos) {
          1w0: prs_mpls;
          1w1: prs_mpls_bos;
          default: accept;
      }
   }

   state prs_mpls_bos {
      transition accept;
   }

   state prs_ipv4 {
      packet.extract(hdr.ipv4);
      transition select(hdr.ipv4.frag_offset, hdr.ipv4.ihl, hdr.ipv4.protocol) {
         (13w0x0, 4w0x5, 8w0x6): prs_tcp;
         (13w0x0, 4w0x5, 8w0x11): prs_udp;
         default: accept;
      } 
   }

   state prs_tcp {
      packet.extract(hdr.tcp);
      md.l3_metadata.lkp_outer_l4_sport = hdr.tcp.src_port;
      md.l3_metadata.lkp_outer_l4_dport = hdr.tcp.dst_port;
      transition select(hdr.tcp.dst_port) {
          16w179: prs_set_prio_med;
          16w639: prs_set_prio_med;
          16w646: prs_set_prio_med;
          default: accept;
      }
   }

  state prs_udp {
     packet.extract(hdr.udp);
     md.l3_metadata.lkp_outer_l4_sport = hdr.udp.src_port;
     md.l3_metadata.lkp_outer_l4_dport = hdr.udp.dst_port;
     transition select(hdr.udp.dst_port) {
           16w67: prs_set_prio_med;
           16w68: prs_set_prio_med;
           16w546: prs_set_prio_med;
           16w547: prs_set_prio_med;
           16w520: prs_set_prio_med;
           16w521: prs_set_prio_med;
           16w646: prs_set_prio_med;
           16w1985: prs_set_prio_med;
           default: accept;
     }
   }

   state prs_llc_header {
      packet.extract(hdr.llc_header);
      transition select(hdr.llc_header.dsap, hdr.llc_header.ssap) {
         /* 
          * (0xaa, 0xaa): prs_snap_header; 
          * From switch.p4 this case should be processed.
          * We are not there yet :-) 
          */
         (0xfe, 0xfe): prs_set_prio_med;
         default: accept;
      }
   }

   state prs_set_prio_med {
      md.intrinsic_metadata.priority = 3;
      transition accept;
   }
}

/*
 * V1Model CHECKSUM VERIFICATION 
 */
control ctl_verify_checksum(inout headers hdr, inout metadata_t metadata) {
    apply {
  }
}


/*
 * V1Model INGRESS
 */
control ctl_ingress(inout headers hdr,
                  inout metadata_t md,
                  inout standard_metadata_t std_md) {

   //action act_rmac_set_nexthop(nexthop_id_t nexthop_id) {
   action act_rmac_set_nexthop() {
      /*
       * Store nexthop value in nexthop_id
       * CPU1:255 => DP1:1
       * CPU2:254 => DP2:2
       */
      md.nexthop_id = CPU_PORT_OFFSET - std_md.ingress_port;
   } 

   /*
    * rmac
    */
   table tbl_rmac_fib {
      key = {
         hdr.ethernet.dst_mac_addr: exact;
      }
      actions = {
         act_rmac_set_nexthop;
         NoAction;
      }
      size = ROUTER_MAC_TABLE_SIZE;
      default_action = NoAction();
   }

   action act_ipv4_cpl_set_nexthop() {
      /*
       * Store nexthop value in nexthop_id
       * CPU1:255 => DP1:1
       * CPU2:254 => DP2:2
       */
      md.nexthop_id = CPU_PORT_OFFSET - std_md.ingress_port;
   } 

   /*
    * IPv4 nexthop processing
    * output value will be the input lkp key of act_nexthop table
    */
   action act_ipv4_set_nexthop(nexthop_id_t nexthop_id) {
      /*
       * Store nexthop value in nexthop_id
       */
      md.nexthop_id = nexthop_id;
   }

   action act_ipv4_mpls_encap_set_nexthop(label_t egress_label, nexthop_id_t nexthop_id) {
      /*
       * Egress packet is now a MPLS packet
       * (LABEL imposition)
       */
      hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
      /*
       * Encapsulate MPLS header
       * And set egress label
       */
      hdr.mpls[0].setValid();
      hdr.mpls[0].label = egress_label;
      /*
       * Set nexthop_id for further forwarding process
       */
      md.nexthop_id = nexthop_id;
   }

   table tbl_ipv4_fib_host {
      key = {
         /*
          * we match /32 host route
          */
         hdr.ipv4.dst_addr: exact;
      }
      actions = {
         act_ipv4_cpl_set_nexthop;
         act_ipv4_set_nexthop;
         act_ipv4_mpls_encap_set_nexthop;
         NoAction;
      }
      size = IPV4_HOST_TABLE_SIZE;
      default_action = NoAction();
   }

   table tbl_ipv4_fib_lpm {
      key = {
         /*
          * we match network route via Long Prefix Match kind operation
          */
         hdr.ipv4.dst_addr: lpm;
      }
      actions = {
         act_ipv4_set_nexthop;
         act_ipv4_mpls_encap_set_nexthop;
         NoAction;
      }
      size = IPV4_LPM_TABLE_SIZE;
      default_action = NoAction();
   }

   action act_mpls_swap_set_nexthop(label_t egress_label, nexthop_id_t nexthop_id) {
      /*
       * Encapsulate MPLS header
       */
      hdr.mpls[0].label = egress_label;
      /*
       * Indicate nexthop_id
       */
      md.nexthop_id = nexthop_id;
   }

   action act_mpls_decap_set_nexthop(nexthop_id_t nexthop_id) {
      /*
       * Egress packet is back now an IPv4 packet
       * (LABEL PHP )
       */
      hdr.ethernet.ethertype = ETHERTYPE_IPV4;
      /*
       * Decapsulate MPLS header
       */
      hdr.mpls[0].setInvalid();
      /*
       * Indicate nexthop_id
       */
      md.nexthop_id = nexthop_id;
   }

   table tbl_mpls_fib {
      key = {
         hdr.mpls[0].label: exact;
      }
      actions = {
         /*
          * mpls core swap 
          */
         act_mpls_swap_set_nexthop;

         /*
          * mpls decapsulation if PHP  
          */
         act_mpls_decap_set_nexthop;

         /* 
          * Default action;
          */
         NoAction;
      }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
    }

   action act_cpl_opr_fib_hit(egress_spec_t egress_port) {
      /*
       * the egress_spec port is now the egress_port
       * set by the control plane entry
       */
      std_md.egress_spec = egress_port;
   }

   /*
    * Perform L3 forwarding
    */
   action act_ipv4_fib_hit(mac_addr_t dst_mac_addr, egress_spec_t egress_port) {
      /*
       * the packet header src_mac is now set to the previous header dst_mac
       */
      hdr.ethernet.src_mac_addr = hdr.ethernet.dst_mac_addr;

      /*
       * the new packet header dst_mac is now the dst_mac 
       * set by the control plane entry
       */
      hdr.ethernet.dst_mac_addr = dst_mac_addr;

      /*
       * the egress_spec port is set now the egress_port 
       * set by the control plane entry
       */
      std_md.egress_spec = egress_port;

      /*
       * We decrement the TTL
       */
      hdr.ipv4.ttl = hdr.ipv4.ttl -1;
   }

   /*
    * Discard via V1Model mark_to_drop(standard_metadata)
    */
   action act_ipv4_fib_discard() {
      mark_to_drop();
   }


   table tbl_nexthop {
      /*
       * custom metadat is used for the lookup key
       */
      key = {
         md.nexthop_id: exact;
         md.intrinsic_metadata.priority: exact;
      }
      actions = {
         act_cpl_opr_fib_hit;
         act_ipv4_fib_hit;
         act_ipv4_fib_discard;
      }
      size = NEXTHOP_TABLE_SIZE;
      default_action = act_ipv4_fib_discard();
   }

   apply {
      /* 
       * if the packet is not valid we don't process it
       * proposed improvement: TTL check <> 0 
       */
      if (hdr.ipv4.isValid()) {
         /*
          * we first consider host routes
          */
         if (!tbl_ipv4_fib_host.apply().hit) {
            /* 
             * if no match consider LPM table
             */
             tbl_ipv4_fib_lpm.apply();
         }
      } 
      else if (hdr.llc_header.isValid()) {
         tbl_rmac_fib.apply();
      } else if (hdr.mpls[0].isValid()) {
           tbl_mpls_fib.apply();
      }
      /*
       * nexthop value is now identified 
       * and stored in custom nexthop_id used for the lookup
       */
      tbl_nexthop.apply();
   }
}

/*
 * V1Model EGRESS
 */

control ctl_egress(inout headers hdr,
                 inout metadata_t md,
                 inout standard_metadata_t std_md) {
   apply {
   }
}

/*
 * V1Model CHECKSUM COMPUTATION
 */
control ctl_compute_checksum(inout headers hdr, inout metadata_t md) {
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

/*
 * V1Model DEPARSER
 */
control ctl_deprs(packet_out packet, in headers hdr) {
    apply {
        /* parsed headers that have been modified
         * in ctl_ingress and ctl_ingress
	 * have to be added again into the packet.
         * for emission in the wire
         */
        /*
        packet.emit(hdr.ethernet);
        packet.emit(hdr.llc_header);
        packet.emit(hdr.ipv4);
        packet.emit(hdr.tcp);
        packet.emit(hdr.udp);
        */
        /*
         * emit hdr
         */
        packet.emit(hdr);
        /*
        packet.emit(hdr.ethernet);
        packet.emit(hdr.ipv4);
        packet.emit(hdr.ipv6);
        packet.emit(hdr.llc_header);
        packet.emit(hdr.tcp);
        packet.emit(hdr.udp);
        packet.emit(hdr.mpls);
        */
    }
}

/*
 * V1Model P4 Switch define in v1model.p4
 */
V1Switch(
prs_main(),
ctl_verify_checksum(),
ctl_ingress(),
ctl_egress(),
ctl_compute_checksum(),
ctl_deprs()
) main;
