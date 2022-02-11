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


class BfNatCounter(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        pipe_name,
        nat_counter_interval=30,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.nat_counter_interval = nat_counter_interval
        self.pipe_name = pipe_name

    def run(self):
        try:
            logger.debug("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - Sending nat counters ..." % self.name)
                logger.debug("%s - %s thread loop" % (self.class_name, self.name))
                self.getReadSwitchNatCounter4()
                self.getReadSwitchNatCounter6()
                sleep(self.nat_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - %s" % (self.class_name, _Exception()))
            self.tearDown()

    def getReadSwitchNatCounter4(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv4_nat_trns_name = "%s.ig_ctl_nat.tbl_ipv4_nat_trns" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv4_nat_trns = self.bfgc.bfrt_info.table_get(tbl_ipv4_nat_trns_name)
            tbl_ipv4_nat_trns.operations_execute(
                self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
            )
            logger.debug(
                "%s - %s counters synced !" % (self.class_name, tbl_ipv4_nat_trns_name)
            )
            resp = tbl_ipv4_nat_trns.entry_get(
                self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    vrf = key_fields["ig_md.vrf"]["value"]
                    prt = key_fields["hdr.ipv4.protocol"]["value"]

                    srcadr = key_fields["hdr.ipv4.src_addr"]["value"]
                    srcprt = key_fields["ig_md.layer4_srcprt"]["value"]
                    trgadr = key_fields["hdr.ipv4.dst_addr"]["value"]
                    trgprt = key_fields["ig_md.layer4_dstprt"]["value"]
                    nat_trns_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    nat_trns_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "nattrns4_cnt %s %s %s %s %s %s %s %s \n" % (
                        vrf,
                        prt,
                        inet_ntoa(srcadr),
                        inet_ntoa(trgadr),
                        srcprt,
                        trgprt,
                        nat_trns_pkt_cnt,
                        nat_trns_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no ipv4 nat entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv4_nat_trns_name)
            )

    def getReadSwitchNatCounter6(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv6_nat_trns_name = "%s.ig_ctl_nat.tbl_ipv6_nat_trns" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv6_nat_trns = self.bfgc.bfrt_info.table_get(tbl_ipv6_nat_trns_name)
            tbl_ipv6_nat_trns.operations_execute(
                self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
            )
            logger.debug(
                "%s - %s counters synced !" % (self.class_name, tbl_ipv6_nat_trns_name)
            )
            resp = tbl_ipv6_nat_trns.entry_get(
                self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
            )

            for d, k in resp:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    vrf = key_fields["ig_md.vrf"]["value"]
                    prt = key_fields["hdr.ipv6.next_hdr"]["value"]
                    srcadr = key_fields["hdr.ipv6.src_addr"]["value"]
                    srcprt = key_fields["ig_md.layer4_srcprt"]["value"]
                    trgadr = key_fields["hdr.ipv6.dst_addr"]["value"]
                    trgprt = key_fields["ig_md.layer4_dstprt"]["value"]
                    nat_trns_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    nat_trns_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    data = "nattrns6_cnt %s %s %s %s %s %s %s %s \n" % (
                        vrf,
                        prt,
                        inet_ntoa(srcadr),
                        inet_ntoa(trgadr),
                        srcprt,
                        trgprt,
                        nat_trns_pkt_cnt,
                        nat_trns_byte_cnt,
                    )

                    logger.debug("tx: %s" % data.split(" "))

                    self.file.write(data)
                    self.file.flush()
            else:
                logger.debug("%s - no ipv6 nat entry" % self.class_name)

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s request problem" % (self.class_name, tbl_ipv6_nat_trns_name)
            )

    def tearDown(self):
        os._exit(0)
        self.die = True
