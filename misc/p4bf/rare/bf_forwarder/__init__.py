from ..bf_gbl_env.var_env import *
from ..api import RareApi

class BfForwarder(Thread,RareApi):
    def __init__(self, threadID, name,platform, bfgc, salgc, sck_file, brdg, mpls, srv6, nat, pbr, tun, poe, mcast, polka, nsh, no_log_keepalive):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.platform = platform
        self.bfgc = bfgc
        self.salgc = salgc
        self.brdg = brdg
        self.mpls = mpls
        self.srv6 = srv6
        self.polka = polka
        self.nsh = nsh
        self.nat = nat
        self.pbr = pbr
        self.tun = tun
        self.poe = poe
        self.mcast = mcast
        self.no_log_keepalive = no_log_keepalive
        self.die=False
        self.hairpins = []
        self.mcast_nid = []
        self.mcast_xid = []
        self.file = sck_file
        self.recirc_port = 68;
        self._clearTable();

    from .message_loop import run

    def tearDown(self):
        os._exit(0);
        self.bfgc.interface._tear_down_stream()

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
        #table_dict[table_name_key].entry_del(self.bfgc.target, keys)
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

