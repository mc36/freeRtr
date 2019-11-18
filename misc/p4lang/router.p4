/*  -*- P4_16 -*- */  

#include <core.p4>
#include <v1model.p4>
#include <include/def_types.p4>
#include <include/cst_cpu_port.p4>
#include <include/cst_ethertype.p4>
#include <include/cst_ip_protocol.p4>
#include <include/cst_table_size.p4>
#include <include/hdr_pkt_in.p4>
#include <include/hdr_pkt_out.p4>
#include <include/hdr_ethernet.p4>
#include <include/hdr_arp.p4>
#include <include/hdr_llc.p4>
#include <include/hdr_vlan.p4> 
#include <include/hdr_mpls.p4>
#include <include/hdr_ipv4.p4>
#include <include/hdr_ipv6.p4>
#include <include/hdr_udp.p4>
#include <include/hdr_tcp.p4>

/*----------------------------------------------------------------------------* 
 *                   I N G R E S S   P R O C E S S I N G                      * 
 *----------------------------------------------------------------------------*/

/*------------------ I N G R E S S  H E A D E R S --------------------------- */ 
#include <include/hdr_ig_headers.p4>                                             

/*------------------ I N G R E S S  G L O B A L  M E T A D A T A ------------ */
#include <include/mtd_ig_metadata.p4>                                           

/*------------------ I N G R E S S   P A R S E R -----------------------------*/
#include <include/ig_prs_main.p4>

/*------------------ V E R I F Y  C H E C K S U M ----------------------------*/
#include <include/ig_ctl_verify_checksum.p4>

/*------------------ I N G R E S S - M A T C H - A C T I O N ---------------- */
#include <include/ig_ctl_vlan_in.p4> 
#include <include/ig_ctl_vlan_out.p4>
#include <include/ig_ctl_vrf.p4>
#include <include/ig_ctl_arp.p4>
#include <include/ig_ctl_llc.p4>
#include <include/ig_ctl_nexthop.p4>
#include <include/ig_ctl_bridge.p4>
#include <include/ig_ctl_mpls.p4>
#include <include/ig_ctl_mpls2.p4>
#include <include/ig_ctl_ipv4.p4>
#include <include/ig_ctl_ipv6.p4>
#include <include/ig_ctl_ipv4b.p4>
#include <include/ig_ctl_ipv6b.p4>
#include <include/ig_ctl_copp.p4>
#include <include/ig_ctl.p4>

/*------------------ E G R E S S - M A T C H - A C T I O N ------------------ */
#include <include/eg_ctl.p4>

/*------------------ C O M P U T E  C H E C K S U M --------------------------*/
#include <include/ig_ctl_compute_checksum.p4>

/*------------------ I N G R E S S  D E P A R S E R ------------------------- */
#include <include/ig_ctl_dprs.p4>

/*------------------ F I N A L  P A C K A G E ------------------------------- */
V1Switch(
   ig_prs_main(),
   ig_ctl_verify_checksum(),
   ig_ctl(),
   eg_ctl(),
   ig_ctl_compute_checksum(),
   ig_ctl_dprs()
) main;
