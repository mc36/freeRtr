


ig_md.always_zero = 0; // hack
ig_md._padding1 = 0;
ig_md._padding2 = 0;
#ifdef HAVE_INQOS
ig_md.inqos_id = 0;
ig_md.inqos_res = MeterColor_t.GREEN;
#endif
#ifdef HAVE_OUTQOS
ig_md.outqos_id = 0;
ig_md.outqos_res = MeterColor_t.GREEN;
#endif
#ifdef HAVE_FLOWSPEC
ig_md.flowspec_id = 0;
ig_md.flowspec_res = MeterColor_t.GREEN;
#endif
#ifdef NEED_REPLICA
ig_md.clone_session = 0;
ig_md.rpf_iface = 0;
#endif
#ifdef HAVE_PPPOE
ig_md.pppoe_ctrl_valid = 0;
ig_md.pppoe_data_valid = 0;
#endif
ig_md.aclport_id = 0;
ig_md.source_id = 0;
ig_md.target_id = 0;
ig_md.nexthop_id = 0;
ig_md.output_id = 0;
#ifdef HAVE_BRIDGE
ig_md.bridge_id = 0;
ig_md.bridge_src = 0;
ig_md.bridge_trg = 0;
#endif
#ifdef NEED_PKTLEN
ig_md.pktlen = 0;
#endif
ig_md.ethertype = 0;
ig_md.vrf = 0;
ig_md.saw_rsvp = 0;
#ifdef HAVE_POLKA
ig_md.polka_next = 0;
#endif
#ifdef HAVE_MPLS
ig_md.mpls_encap_egress_label = 0;
ig_md.mpls_encap_svc_label = 0;
#endif
#ifdef HAVE_SRV6
ig_md.srv_target = 0;
#endif
#ifdef HAVE_NAT
ig_md.checksum_tcp_tmp = 0;
ig_md.checksum_udp_tmp = 0;
#endif
ig_md.mpls_op_type = 0;
ig_md.srv_op_type = 0;
ig_md.bier_remove = 0;
ig_md.polka_remove = 0;
ig_md.nsh_remove = 0;
ig_md.mpls0_remove = 0;
ig_md.mpls1_remove = 0;
ig_md.srv_remove = 0;
#ifdef HAVE_POLKA
ig_md.polka_valid = 0;
#endif
#ifdef HAVE_NSH
ig_md.nsh_valid = 0;
#endif
#ifdef HAVE_SGT
ig_md.sgt_valid = 0;
ig_md.sec_grp_id = 0;
#endif
#ifdef HAVE_GTP
ig_md.gtp_type = 0;
#endif
#ifdef HAVE_MPLS
ig_md.mpls0_valid = 0;
ig_md.mpls1_valid = 0;
#endif
#ifdef HAVE_NAT
ig_md.natted_ipv4tcp = 0;
ig_md.natted_ipv4udp = 0;
ig_md.natted_ipv6tcp = 0;
ig_md.natted_ipv6udp = 0;
#endif
ig_md.arp_valid = 0;
ig_md.ipv4_valid = 0;
ig_md.ipv6_valid = 0;
ig_md.layer4_srcprt = 0;
ig_md.layer4_dstprt = 0;
ig_md.srv_encap_l3vpn_valid = 0;
ig_md.mpls_encap_rawip_valid = 0;
ig_md.mpls_encap_l3vpn_valid = 0;
ig_md.mpls_encap_l2vpn_valid = 0;
ig_md.mpls_encap_xconnect_valid = 0;
ig_md.mpls_encap_decap_sap_type = 0;
ig_md.l4_lookup = { 0, 0 };


