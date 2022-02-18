from ..bf_gbl_env.var_env import *
from ..api import RareApi


class BfForwarder(Thread, RareApi):
    def __init__(
        self,
        threadID,
        name,
        platform,
        bfgc,
        salgc,
        sckr_file,
        sckw_file,
        no_log_keepalive,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.platform = platform
        self.bfgc = bfgc
        self.salgc = salgc
        self.no_log_keepalive = no_log_keepalive
        self.die = False
        self.hairpins = []
        self.mcast_nid = []
        self.mcast_xid = []
        self.file_r = sckr_file
        self.file_w = sckw_file
        self.recirc_port = 68
        self.dp_capabilities = {}
        self._clearTable()

    from .message_loop import run

    def tearDown(self):
        os._exit(0)
        self.bfgc.interface.tear_down_stream()

    def _getTableKeys(self, table_name_key, table_dict, keys):
        try:
            for (d, k) in table_dict[table_name_key].entry_get(self.bfgc.target):
                if k is not None:
                    keys.append(k)
        except Exception as e:
            logger.warning("Error cleaning up: {}".format(e))
        logger.debug("   Keys to delete: %s" % (keys))

    def _delTableKeys(self, table_name_key, table_dict, keys):
        print("  Clearing Table {}".format(table_name_key))

        for k in keys:
            try:
                logger.debug("   Key to delete: %s" % (k))
                table_dict[table_name_key].entry_del(self.bfgc.target, [k])
            except Exception as e:
                logger.warning("Error cleaning up: {}".format(e))

    def _clearOneTable(self, table_name_key, table_dict):
        keys = []
        self._getTableKeys(table_name_key, table_dict, keys)
        self._delTableKeys(table_name_key, table_dict, keys)

    def _clearTable(self):
        table_dict = {}
        for (
            table_name,
            table_info,
        ) in self.bfgc.bfrt_info.parsed_info.table_info_dict_get().items():
            table_dict[table_info.name_get()] = gc._Table(
                table_info, self.bfgc.interface.reader_writer_interface
            )

        logger.warning("Generic table clearing: (Order not matters)")

        for table_name_key in table_dict.keys():
            if "ig_ctl_pkt_pre_emit" in table_name_key:
                continue
            if "ig_ctl_bundle" in table_name_key:
                continue
            # IN/OUT meters cannot be cleared
            if "policer" in table_name_key:
                continue
            if "hash" in table_name_key:
                continue
            # IN/OUT counters cannot be cleared
            if "stats" in table_name_key:
                continue
            if "stats4" in table_name_key:
                continue
            if "stats6" in table_name_key:
                continue
            if "ig_ctl" in table_name_key:
                self._clearOneTable(table_name_key, table_dict)
            if "eg_ctl" in table_name_key:
                self._clearOneTable(table_name_key, table_dict)

        logger.warning("Bundle specific clearing: (Order matters)")

        nhp_name = "%s.ig_ctl.ig_ctl_bundle.tbl_nexthop_bundle" % self.bfgc.pipe_name
        ase_name = "%s.ig_ctl.ig_ctl_bundle.ase_bundle" % self.bfgc.pipe_name
        apr_name = "%s.ig_ctl.ig_ctl_bundle.apr_bundle" % self.bfgc.pipe_name
        nhp_keys = []
        ase_keys = []
        apr_keys = []
        self._getTableKeys(nhp_name, table_dict, nhp_keys)
        self._getTableKeys(ase_name, table_dict, ase_keys)
        self._getTableKeys(apr_name, table_dict, apr_keys)
        self._delTableKeys(nhp_name, table_dict, nhp_keys)
        self._delTableKeys(ase_name, table_dict, ase_keys)
        self._delTableKeys(apr_name, table_dict, apr_keys)

        logger.warning("Multicast specific clearing: (Order matters)")

        mgid_name = "$pre.mgid"
        node_name = "$pre.node"
        mgid_keys = []
        node_keys = []
        self._getTableKeys(mgid_name, table_dict, mgid_keys)
        self._getTableKeys(node_name, table_dict, node_keys)
        self._delTableKeys(mgid_name, table_dict, mgid_keys)
        self._delTableKeys(node_name, table_dict, node_keys)

        self._getDataplaneCapability()
        self._setDataplaneCapability()

        pltfm = "platform tna/%s\n" % self.class_name.lower()
        self.file_w.write(pltfm)
        self.file_w.flush()

        data = "capabilities "
        for capability in self.dp_capabilities.keys():
            if self.dp_capabilities[capability] == True:
                data = data + capability + " "
        self.file_w.write(data + "\n")
        self.file_w.flush()
        logger.debug("tx: %s" % data)

    def _getDataplaneCapability(self):
        capabilities = {
            "mpls": "ig_ctl.ig_ctl_mpls.tbl_mpls_fib",
            "bridge": "ig_ctl.ig_ctl_bridge.tbl_bridge_target",
            "copp": "ig_ctl.ig_ctl_copp.tbl_ipv4_copp",
            "flowspec": "ig_ctl.ig_ctl_flowspec.tbl_ipv4_flowspec",
            "mcast": "eg_ctl.eg_ctl_mcast.tbl_mcast",
            "nat": "ig_ctl.ig_ctl_nat.tbl_ipv4_nat_trns",
            "polka": "ig_ctl.ig_ctl_polka.tbl_polka",
            "pbr": "ig_ctl.ig_ctl_pbr.tbl_ipv4_pbr",
            "pppoe": "ig_ctl.ig_ctl_pppoe.tbl_pppoe",
            "nsh": "ig_ctl.ig_ctl_nsh.tbl_nsh",
            "inacl": "ig_ctl.ig_ctl_acl_in.tbl_ipv4_acl",
            "outacl": "ig_ctl.ig_ctl_acl_out.tbl_ipv4_acl",
            "inqos": "ig_ctl.ig_ctl_qos_in.tbl_ipv4_qos",
            "outqos": "ig_ctl.ig_ctl_qos_out.tbl_ipv4_qos",
            "tun": "ig_ctl.ig_ctl_tunnel.tbl_tunnel4",
            "srv6": "ig_ctl.ig_ctl_ipv6b.tbl_ipv6_fib_host",
        }
        sub_capabilities = {
            "bier": {
                "parent": {"id": "mpls", "path": "ig_ctl_mpls.tbl_mpls_fib"},
                "action": "ig_ctl.ig_ctl_mpls.act_mpls_bier_label",
            },
            "duplab": {
                "parent": {"id": "mpls", "path": "ig_ctl_mpls.tbl_mpls_fib"},
                "action": "ig_ctl.ig_ctl_mpls.act_mpls_bcast_label",
            },
            "gre": {
                "parent": {"id": "tun", "path": "ig_ctl_tunnel.tbl_tunnel4"},
                "action": "ig_ctl.ig_ctl_tunnel.act_tunnel_gre",
            },
            "ipip": {
                "parent": {"id": "tun", "path": "ig_ctl_tunnel.tbl_tunnel4"},
                "action": "ig_ctl.ig_ctl_tunnel.act_tunnel_ip4ip",
            },
            "pckoudp": {
                "parent": {"id": "tun", "path": "ig_ctl_tunnel.tbl_tunnel4"},
                "action": "ig_ctl.ig_ctl_tunnel.act_tunnel_pckoudp",
            },
            "vxlan": {
                "parent": {"id": "tun", "path": "ig_ctl_tunnel.tbl_tunnel4"},
                "action": "ig_ctl.ig_ctl_tunnel.act_tunnel_vxlan",
            },
            "l2tp": {
                "parent": {"id": "tun", "path": "ig_ctl_tunnel.tbl_tunnel4"},
                "action": "ig_ctl.ig_ctl_tunnel.act_tunnel_l2tp",
            },
            "tap": {
                "parent": {"id": "bridge", "path": "ig_ctl_bridge.tbl_bridge_target"},
                "action": "ig_ctl.ig_ctl_bridge.act_set_bridge_routed",
            },
            "inspect_in": {
                "parent": {"id": "inacl", "path": "ig_ctl_acl_in.tbl_ipv4_insp"},
                "action": "ig_ctl.ig_ctl_acl_in.act_insp4",
            },
            "inspect_out": {
                "parent": {"id": "outacl", "path": "ig_ctl_acl_out.tbl_ipv4_insp"},
                "action": "ig_ctl.ig_ctl_acl_out.act_insp4",
            },
            "racl": {
                "parent": {"id": "inacl", "path": "ig_ctl_acl_in.tbl_ipv4_acl"},
                "action": "ig_ctl.ig_ctl_acl_in.act_punt",
            },
        }

        ig_ctl_path = "%s.ig_ctl" % self.bfgc.pipe_name
        tbl_list = self.bfgc.bfrt_info.table_dict.keys()
        for capability in capabilities.keys():
            capability_name = "%s" % (capabilities[capability])
            if capability_name in tbl_list:
                self.dp_capabilities[capability] = True
                logger.debug("%s - %s supported" % (self.class_name, capability))
            else:
                self.dp_capabilities[capability] = False
                logger.debug("%s - %s not supported" % (self.class_name, capability))

        for sub_capability in sub_capabilities.keys():
            capability_name = "%s.%s" % (
                ig_ctl_path,
                sub_capabilities[sub_capability]["parent"]["path"],
            )

            logger.debug(
                "%s - capability name %s " % (self.class_name, capability_name)
            )
            act_sub_capability_name = "%s" % (
                sub_capabilities[sub_capability]["action"]
            )
            logger.debug(
                "%s - action capability name %s "
                % (self.class_name, act_sub_capability_name)
            )
            if self.dp_capabilities[sub_capabilities[sub_capability]["parent"]["id"]]:
                try:
                    tbl_capability = self.bfgc.bfrt_info.table_get(capability_name)
                except Exception as e:
                    self.dp_capabilities[sub_capability] = False
                    logger.debug(
                        "%s - cannot fetch table %s for %s not supported: %s "
                        % (self.class_name, capability_name, sub_capability, e)
                    )

                logger.debug(
                    "%s - table %s action list %s "
                    % (
                        self.class_name,
                        capability_name,
                        tbl_capability.info.action_dict.keys(),
                    )
                )

                if act_sub_capability_name in tbl_capability.info.action_dict.keys():
                    logger.debug(
                        "%s - %s supported" % (self.class_name, sub_capability)
                    )
                    self.dp_capabilities[sub_capability] = True
                else:
                    logger.debug(
                        "%s - %s not supported"
                        % (
                            self.class_name,
                            sub_capability,
                        )
                    )
                    self.dp_capabilities[sub_capability] = False
            else:
                logger.debug(
                    "%s - %s not supported"
                    % (
                        self.class_name,
                        sub_capability,
                    )
                )
                self.dp_capabilities[sub_capability] = False

    def _setDataplaneCapability(self):
        for capability in self.dp_capabilities:
            self.__dict__[capability] = self.dp_capabilities[capability]
