/*  -*- P4_16 -*- */

#include <core.p4>
#include <tna.p4>
#include "include/cst_bundle.p4"
#include "include/cst_cpu_port.p4"
#include "include/cst_ethertype.p4"
#include "include/cst_ip_protocol.p4"
#include "include/cst_table_size.p4"
#include "include/def_types.p4"
#include "include/hdr_cpu.p4"
#include "include/hdr_ethernet.p4"
#include "include/hdr_arp.p4"
#include "include/hdr_llc.p4"
#include "include/hdr_vlan.p4"
#include "include/hdr_mpls.p4"
#include "include/hdr_ipv4.p4"
#include "include/hdr_ipv6.p4"
#include "include/hdr_tcp.p4"
#include "include/hdr_udp.p4"
#include "include/hdr_gre.p4"
#include "include/hdr_pppoe.p4"
#include "include/hdr_l2tp.p4"

/*----------------------------------------------------------------------------*
 *                   I N G R E S S   P R O C E S S I N G                      *
 *----------------------------------------------------------------------------*/

/*------------------ I N G R E S S  H E A D E R S --------------------------- */
#include "include/hdr_ig_headers.p4"

/*------------------ I N G R E S S  G L O B A L  M E T A D A T A ------------ */
#include "include/mtd_ig_metadata.p4"

/*------------------ I N G R E S S   P A R S E R -----------------------------*/
#include "include/ig_prs_main.p4"

/*------------------ I N G R E S S - M A T C H - A C T I O N ---------------- */
/* Include the proper hashing module */
#if HASHING == IPV4_IPV6_HASH
#include "include/hsh_ipv4_ipv6_hash.p4"
#elif HASHING == RANDOM_HASH
#include "include/hsh_random_hash.p4"
#elif HASHING == ROUND_ROBIN_HASH
#include "include/hsh_round_robin_hash.p4"
#elif HASHING == NO_HASH
#include "include/hsh_no_hash.p4"
#else
#error Unknown hashing module (HASHING)
#endif
#include "include/ig_ctl_bundle.p4"
#include "include/ig_ctl_pkt_pre_emit.p4"
#include "include/ig_ctl_vlan_in.p4"
#include "include/ig_ctl_vlan_out.p4"
#include "include/ig_ctl_acl_in.p4"
#include "include/ig_ctl_acl_out.p4"
#include "include/ig_ctl_vrf.p4"
#include "include/ig_ctl_nexthop.p4"
#include "include/ig_ctl_bridge.p4"
#include "include/ig_ctl_mpls.p4"
#include "include/ig_ctl_ipv4.p4"
#include "include/ig_ctl_ipv6.p4"
#include "include/ig_ctl_ipv4b.p4"
#include "include/ig_ctl_ipv6b.p4"
#include "include/ig_ctl_nat.p4"
#include "include/ig_ctl_tunnel.p4"
#include "include/ig_ctl_pppoe.p4"
#include "include/ig_ctl_copp.p4"
#include "include/ig_ctl.p4"

/*------------------ I N G R E S S  D E P A R S E R ------------------------- */
#include "include/ig_ctl_dprs.p4"

/*----------------------------------------------------------------------------*
 *                   E G R E S S   P R O C E S S I N G                        *
 *----------------------------------------------------------------------------*/

/*------------------ E G R E S S  H E A D E R S ----------------------------- */
#include "include/hdr_eg_headers.p4"

/*------------------ E G R E S S  G L O B A L  M E T A D A T A -------------- */
#include "include/mtd_eg_metadata.p4"

/*------------------ E G R E S S  P A R S E R ------------------------------- */
#include "include/eg_prs_main.p4"

/*------------------ E G R E S S  M A T C H - A C T I O N ------------------- */
#include "include/eg_ctl.p4"

/*------------------ E G R E S S  D E P A R S E R --------------------------- */
#include "include/eg_ctl_dprs.p4"

/*------------------ F I N A L  P A C K A G E ------------------------------- */

Pipeline(
    ig_prs_main(),
    ig_ctl(),
    ig_ctl_dprs(),
    eg_prs_main(),
    eg_ctl(),
    eg_ctl_dprs()
) pipe;

Switch(pipe) main;

