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


class BfAclCounter(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        pipe_name,
        acl_counter_interval=30,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.acl_counter_interval = acl_counter_interval
        self.pipe_name = pipe_name

    def run(self):
        try:
            logger.debug("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - Sending acl counters ..." % self.name)
                logger.debug("%s - %s thread loop" % (self.class_name, self.name))
                self.getReadSwitchAclInCounter4()
                self.getReadSwitchAclInCounter6()
                self.getReadSwitchAclOutCounter4()
                self.getReadSwitchAclOutCounter6()
                sleep(self.acl_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - %s" % (self.class_name, _Exception()))
            self.tearDown()

    def getReadSwitchAclInCounter4(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv4_acl_name = "%s.ig_ctl_acl_in.tbl_ipv4_acl" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv4_acl = self.bfgc.bfrt_info.table_get(tbl_ipv4_acl_name)

            resp = tbl_ipv4_acl.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    port_id = key_fields["ig_md.source_id"]["value"]
                    pri = key_fields["$MATCH_PRIORITY"]["value"]

                    acl_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    acl_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "inacl4_cnt %s %s %s %s \n" % (
                        port_id,
                        pri,
                        acl_pkt_cnt,
                        acl_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no ingress ipv4 acl entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv4_acl_name)
            )

    def getReadSwitchAclInCounter6(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv6_acl_name = "%s.ig_ctl_acl_in.tbl_ipv6_acl" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv6_acl = self.bfgc.bfrt_info.table_get(tbl_ipv6_acl_name)

            resp = tbl_ipv6_acl.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    port_id = key_fields["ig_md.source_id"]["value"]
                    pri = key_fields["$MATCH_PRIORITY"]["value"]
                    acl_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    acl_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "inacl6_cnt %s %s %s %s \n" % (
                        port_id,
                        pri,
                        acl_pkt_cnt,
                        acl_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no ingress ipv6 acl entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv6_acl_name)
            )

    def getReadSwitchAclOutCounter4(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv4_acl_name = "%s.ig_ctl_acl_out.tbl_ipv4_acl" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv4_acl = self.bfgc.bfrt_info.table_get(tbl_ipv4_acl_name)

            resp = tbl_ipv4_acl.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    port_id = key_fields["ig_md.aclport_id"]["value"]
                    pri = key_fields["$MATCH_PRIORITY"]["value"]

                    acl_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    acl_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "outacl4_cnt %s %s %s %s \n" % (
                        port_id,
                        pri,
                        acl_pkt_cnt,
                        acl_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no egress ipv4 acl entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv4_acl_name)
            )

    def getReadSwitchAclOutCounter6(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv6_acl_name = "%s.ig_ctl_acl_out.tbl_ipv6_acl" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv6_acl = self.bfgc.bfrt_info.table_get(tbl_ipv6_acl_name)
            resp = tbl_ipv6_acl.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    port_id = key_fields["ig_md.aclport_id"]["value"]
                    pri = key_fields["$MATCH_PRIORITY"]["value"]

                    acl_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    acl_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "outacl6_cnt %s %s %s %s \n" % (
                        port_id,
                        pri,
                        acl_pkt_cnt,
                        acl_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no egress ipv6 acl entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv6_acl_name)
            )

    def tearDown(self):
        os._exit(0)
        self.die = True
