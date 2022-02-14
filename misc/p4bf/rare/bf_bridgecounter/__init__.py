from ..bf_gbl_env.var_env import *


def _Exception():
    exc_type, exc_obj, tb = sys.exc_info()
    f = tb.tb_frame
    lineno = tb.tb_lineno
    filename = f.f_code.co_filename
    linecache.checkcache(filename)
    line = linecache.getline(filename, lineno, f.f_globals)
    return 'Exception in ({},\n line {} "{}"): {}'.format(
        filename, lineno, line.strip(), exc_obj
    )


class BfBridgeCounter(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        pipe_name,
        bridge_counter_interval=30,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.bridge_counter_interval = bridge_counter_interval
        self.pipe_name = pipe_name

    def run(self):
        try:
            logger.debug("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - %s thread loop" % (self.class_name, self.name))
                self.getReadSwitchBridgeCounter()
                sleep(self.bridge_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - %s" % (self.class_name, _Exception()))
            self.tearDown()

    def getReadSwitchBridgeCounter(self):
        logger.debug("%s - Sending bridge counters ..." % self.name)
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_bridge_learn_name = "%s.ig_ctl_bridge.tbl_bridge_learn" % (tbl_global_path)
        tbl_bridge_target_name = "%s.ig_ctl_bridge.tbl_bridge_target" % (
            tbl_global_path
        )

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_bridge_learn = self.bfgc.bfrt_info.table_get(tbl_bridge_learn_name)
            tbl_bridge_learn.operations_execute(
                self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
            )
            tbl_bridge_target = self.bfgc.bfrt_info.table_get(tbl_bridge_target_name)
            tbl_bridge_target.operations_execute(
                self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
            )
            logger.debug(
                "%s - %s and %s counters synced !"
                % (self.class_name, tbl_bridge_learn_name, tbl_bridge_target_name)
            )
            resp_learn = tbl_bridge_learn.entry_get(
                self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
            )
            resp_target = tbl_bridge_target.entry_get(
                self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
            )

            bridge_rx_dict = {}
            bridge_tx_dict = {}

            for d, k in resp_learn:
                key_fields_learn = k.to_dict()
                data_fields_learn = d.to_dict()

                if not (key_fields_learn == {}):
                    bridge_id_learn = key_fields_learn["ig_md.bridge_id"]["value"]
                    src_mac_learn = key_fields_learn["hdr.ethernet.src_mac_addr"][
                        "value"
                    ]
                    bridge_rx_pkt_cnt = data_fields_learn["$COUNTER_SPEC_PKTS"]
                    bridge_rx_byte_cnt = data_fields_learn["$COUNTER_SPEC_BYTES"]
                    ntry_key = "%s %s" % (bridge_id_learn, mac_ntoa(src_mac_learn))
                    bridge_rx_dict[ntry_key] = "%s %s" % (
                        bridge_rx_pkt_cnt,
                        bridge_rx_byte_cnt,
                    )
                else:
                    logger.debug(
                        "%s - no mac entry in %s"
                        % (self.class_name, tbl_bridge_learn_name)
                    )

            for d, k in resp_target:
                key_fields_target = k.to_dict()
                data_fields_target = d.to_dict()

                if not (key_fields_target == {}):
                    bridge_id_target = key_fields_target["ig_md.bridge_id"]["value"]
                    dst_mac_target = key_fields_target["hdr.ethernet.dst_mac_addr"][
                        "value"
                    ]
                    bridge_tx_pkt_cnt = data_fields_target["$COUNTER_SPEC_PKTS"]
                    bridge_tx_byte_cnt = data_fields_target["$COUNTER_SPEC_BYTES"]
                    ntry_key = "%s %s" % (bridge_id_target, mac_ntoa(dst_mac_target))
                    bridge_tx_dict[ntry_key] = "%s %s" % (
                        bridge_tx_pkt_cnt,
                        bridge_tx_byte_cnt,
                    )
                else:
                    logger.debug(
                        "%s - no mac entry in %s"
                        % (self.class_name, tbl_bridge_target_name)
                    )

            if len(bridge_rx_dict.keys()) != 0:
                for key in bridge_rx_dict.keys():
                    if key not in bridge_tx_dict.keys():
                        data = "bridge_cnt %s %s 0 0 \n" % (key, bridge_rx_dict[key])
                    else:
                        data = "bridge_cnt %s %s %s \n" % (
                            key,
                            bridge_rx_dict[key],
                            bridge_tx_dict[key],
                        )
                        bridge_tx_dict.pop(key)

                    logger.debug("tx: %s" % data.split(" "))
                    self.file.write(data)
                    self.file.flush()

            if len(bridge_tx_dict.keys()) != 0:
                for key in bridge_tx_dict.keys():
                    data = "brige_cnt %s 0 0 %s \n" % (key, bridge_tx_dict[key])
                    logger.debug("tx: %s" % data.split(" "))
                    self.file.write(data)
                    self.file.flush()
        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - no mac entry in %s and %s ..."
                % (self.class_name, tbl_bridge_learn_name, tbl_bridge_target_name)
            )

    def tearDown(self):
        os._exit(0)
        self.die = True
