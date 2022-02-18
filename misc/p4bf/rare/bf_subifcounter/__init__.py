from ..bf_gbl_env.var_env import *


def _Exception():
    exc_type, exc_obj, tb = sys.exc_info()
    f = tb.tb_frame
    lineno = tb.tb_lineno
    filename = f.f_code.co_filename
    linecache.checkcache(filename)
    line = linecache.getline(filename, lineno, f.f_globals)
    return 'EXCEPTION IN ({}, LINE {} "{}"): {}'.format(
        filename, lineno, line.strip(), exc_obj
    )


class BfSubIfCounter(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        pipe_name,
        subif_counter_interval,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.subif_counter_interval = subif_counter_interval
        self.pipe_name = pipe_name
        self.active_ports = {}
        self.active_subifs = {}
        self.subif_counters = {}

    def getAllActivePorts(self):
        resp = self.bfgc.port_table.entry_get(
            self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
        )
        ACTIVE_PORTS = {}
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            ACTIVE_PORTS[key_fields["$DEV_PORT"]["value"]] = data_fields["$PORT_NAME"]
        self.active_ports = ACTIVE_PORTS

    def run(self):
        try:
            logger.warning("%s - main" % (self.class_name))
            while not self.die:
                self.getAllActivePorts()
                logger.debug("%s - loop" % (self.class_name))
                if len(self.active_ports.keys()) == 0:
                    logger.debug("%s - No active ports" % (self.class_name))
                else:
                    logger.debug(
                        "%s - ACTIVE_PORTS%s"
                        % (self.class_name, self.active_ports.keys())
                    )
                    self.getReadSwitchSubIfCounter()
                sleep(self.subif_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning(
                "%s - exited with code [%s]" % (self.class_name, _Exception())
            )
            self.tearDown()

    def getReadSwitchSubIfCounter(self):
        # for all subif
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_global2_path = "%s.eg_ctl" % self.pipe_name
        tbl_name_vlan_in = "%s.ig_ctl_vlan_in.tbl_vlan_in" % (tbl_global_path)
        tbl_vlan_in = self.bfgc.bfrt_info.table_get(tbl_name_vlan_in)
        tbl_name_in = "%s.ig_ctl_vlan_in.stats" % (tbl_global_path)
        tbl_stats_in = self.bfgc.bfrt_info.table_get(tbl_name_in)
        tbl_name_out = "%s.eg_ctl_vlan_out.stats" % (tbl_global2_path)
        tbl_stats_out = self.bfgc.bfrt_info.table_get(tbl_name_out)
        tbl_name_pkt_out = "%s.pkt_out_stats" % (tbl_global_path)
        tbl_stats_pkt_out = self.bfgc.bfrt_info.table_get(tbl_name_pkt_out)
        logger.debug("INGRESS STATS TABLE PATH: %s" % tbl_name_in)
        logger.debug("EGRESS STATS TABLE PATH: %s" % tbl_name_out)
        logger.debug("EGRESS PKT_OUT_STATS TABLE PATH: %s" % tbl_name_pkt_out)

        # tbl_stats_in.operations_execute(
        #    self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
        # )
        # tbl_stats_out.operations_execute(
        #    self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
        # )
        # tbl_stats_pkt_out.operations_execute(
        #    self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
        # )

        # resp = tbl_vlan_in.entry_get(
        #    self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
        # )
        resp = tbl_vlan_in.entry_get(
            self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
        )
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            self.active_subifs[data_fields["src"]] = [
                key_fields["ig_md.ingress_id"]["value"],
                key_fields["hdr.vlan.vid"]["value"],
            ]

        # for port_id in self.active_ports.keys():
        #    self.subif_counters.update({port_id:[0,0,0,0]})
        for subif_if in self.active_subifs.keys():
            self.subif_counters.update({subif_if: [0, 0, 0, 0]})

        for key in self.subif_counters.keys():
            logger.debug("SUBIF_COUNTERS[%s]=%s" % (key, self.subif_counters[key]))

        for counter_id in self.subif_counters.keys():
            # read counter from p4 switch via grpc client
            logger.debug("%s - reading counter_id[%s]" % (self.class_name, counter_id))
            # IN counters
            key_list = [
                tbl_stats_in.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])
            ]
            data_list = None
            stats_in_entry = tbl_stats_in.entry_get(
                self.bfgc.target,
                key_list,
                {"from_hw": True},
                data_list,
                p4_name=self.bfgc.p4_name,
            )

            stats_in = next(stats_in_entry)[0].to_dict()
            logger.debug("INGRESS STATS FOR SUBIF[%s]=%s" % (counter_id, stats_in))

            # OUT counters
            key_list = [
                tbl_stats_out.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])
            ]
            data_list = None
            stats_out_entry = tbl_stats_out.entry_get(
                self.bfgc.target,
                key_list,
                {"from_hw": True},
                data_list,
                p4_name=self.bfgc.p4_name,
            )
            stats_out = next(stats_out_entry)[0].to_dict()
            logger.debug("EGRESS STATS FOR SUBIF[%s]=%s" % (counter_id, stats_out))

            # PKT_OUT counters
            key_list = [
                tbl_stats_pkt_out.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])
            ]
            data_list = None
            stats_pkt_out_entry = tbl_stats_pkt_out.entry_get(
                self.bfgc.target,
                key_list,
                {"from_hw": True},
                data_list,
                p4_name=self.bfgc.p4_name,
            )

            stats_pkt_out = next(stats_pkt_out_entry)[0].to_dict()
            logger.debug(
                "EGRESS PKT_OUT STATS FOR SUBIF[%s]=%s" % (counter_id, stats_pkt_out)
            )

            self.subif_counters.update(
                {
                    counter_id: [
                        stats_in["$COUNTER_SPEC_PKTS"]
                        - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                        stats_in["$COUNTER_SPEC_BYTES"]
                        - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                        stats_out["$COUNTER_SPEC_PKTS"]
                        + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                        stats_out["$COUNTER_SPEC_BYTES"]
                        + stats_pkt_out["$COUNTER_SPEC_BYTES"],
                    ]
                }
            )

            logger.debug(
                "tx: ['counter','%s','%s','%s', '%s', '%s'\\n']"
                % (
                    counter_id,
                    stats_in["$COUNTER_SPEC_PKTS"]
                    - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                    stats_in["$COUNTER_SPEC_BYTES"]
                    - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                    stats_out["$COUNTER_SPEC_PKTS"]
                    + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                    stats_out["$COUNTER_SPEC_BYTES"]
                    + stats_pkt_out["$COUNTER_SPEC_BYTES"],
                )
            )

            self.file.write(
                "counter %s %s %s %s %s\n"
                % (
                    counter_id,
                    stats_in["$COUNTER_SPEC_PKTS"]
                    - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                    stats_in["$COUNTER_SPEC_BYTES"]
                    - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                    stats_out["$COUNTER_SPEC_PKTS"]
                    + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                    stats_out["$COUNTER_SPEC_BYTES"]
                    + stats_pkt_out["$COUNTER_SPEC_BYTES"],
                )
            )
            self.file.flush()

    def tearDown(self):
        os._exit(0)
        self.die = True
