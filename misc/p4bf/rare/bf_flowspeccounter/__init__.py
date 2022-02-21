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


class BfFlowspecCounter(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        pipe_name,
        flowspec_counter_interval=30,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.flowspec_counter_interval = flowspec_counter_interval
        self.pipe_name = pipe_name

    def run(self):
        try:
            logger.debug("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - Sending flowspec counters ..." % self.name)
                logger.debug("%s - %s thread loop" % (self.class_name, self.name))
                self.getReadSwitchFlowspecCounter4()
                self.getReadSwitchFlowspecCounter6()
                sleep(self.flowspec_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - %s" % (self.class_name, _Exception()))
            self.tearDown()

    def getReadSwitchFlowspecCounter4(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv4_flowspec_name = "%s.ig_ctl_flowspec.tbl_ipv4_flowspec" % (
            tbl_global_path
        )

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv4_flowspec = self.bfgc.bfrt_info.table_get(tbl_ipv4_flowspec_name)

            resp = tbl_ipv4_flowspec.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    vrf = key_fields["ig_md.vrf"]["value"]
                    pri = key_fields["$MATCH_PRIORITY"]["value"]

                    flowspec_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    flowspec_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "flowspec4_cnt %s %s %s %s \n" % (
                        vrf,
                        pri,
                        flowspec_pkt_cnt,
                        flowspec_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no ipv4 flowspec entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv4_flowspec_name)
            )

    def getReadSwitchFlowspecCounter6(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv6_flowspec_name = "%s.ig_ctl_flowspec.tbl_ipv6_flowspec" % (
            tbl_global_path
        )

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv6_flowspec = self.bfgc.bfrt_info.table_get(tbl_ipv6_flowspec_name)

            resp = tbl_ipv6_flowspec.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    vrf = key_fields["ig_md.vrf"]["value"]
                    pri = key_fields["$MATCH_PRIORITY"]["value"]

                    flowspec_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    flowspec_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "flowspec6_cnt %s %s %s %s \n" % (
                        vrf,
                        pri,
                        flowspec_pkt_cnt,
                        flowspec_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no ipv6 flowspec entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv6_flowspec_name)
            )

    def tearDown(self):
        os._exit(0)
        self.die = True
