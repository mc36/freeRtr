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


class BfInspectCounter(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        pipe_name,
        inspect_counter_interval=30,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.inspect_counter_interval = inspect_counter_interval
        self.pipe_name = pipe_name

    def run(self):
        try:
            logger.debug("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - Sending inspect counters ..." % self.name)
                logger.debug("%s - %s thread loop" % (self.class_name, self.name))
                self.getReadSwitchInspectCounter4()
                self.getReadSwitchInspectCounter6()
                sleep(self.inspect_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - %s" % (self.class_name, _Exception()))
            self.tearDown()

    def getReadSwitchInspectCounter4(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv4_inspect_rx_name = "%s.ig_ctl_acl_in.tbl_ipv4_insp" % (tbl_global_path)
        tbl_ipv4_inspect_tx_name = "%s.ig_ctl_acl_out.tbl_ipv4_insp" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv4_inspect_rx = self.bfgc.bfrt_info.table_get(
                tbl_ipv4_inspect_rx_name
            )
            tbl_ipv4_inspect_tx = self.bfgc.bfrt_info.table_get(
                tbl_ipv4_inspect_tx_name
            )
            resp_rx = tbl_ipv4_inspect_rx.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )
            resp_tx = tbl_ipv4_inspect_tx.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            ipv4_inspect_rx_dict = {}
            ipv4_inspect_tx_dict = {}

            for d, k in resp_rx:
                key_fields = {}
                data_fields = {}
                data = ""

                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    src_id = key_fields["ig_md.source_id"]["value"]
                    prt = key_fields["hdr.ipv4.protocol"]["value"]
                    srcadr = key_fields["hdr.ipv4.src_addr"]["value"]
                    srcprt = key_fields["ig_md.layer4_srcprt"]["value"]
                    dstadr = key_fields["hdr.ipv4.dst_addr"]["value"]
                    dstprt = key_fields["ig_md.layer4_dstprt"]["value"]
                    ipv4_insp_rx_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    ipv4_insp_rx_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    ntry_key = "%s %s %s %s %s %s" % (
                        src_id,
                        prt,
                        inet_ntoa(srcadr),
                        inet_ntoa(dstadr),
                        srcprt,
                        dstprt,
                    )

                    ipv4_inspect_rx_dict[ntry_key] = "%s %s" % (
                        ipv4_insp_rx_pkt_cnt,
                        ipv4_insp_rx_byte_cnt,
                    )

                else:
                    logger.debug(
                        "%s - no ipv4 ingress inspect entry in %s"
                        % (self.class_name, tbl_ipv4_inspect_rx_name)
                    )

            for d, k in resp_tx:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    aclport_id = key_fields["ig_md.aclport_id"]["value"]
                    prt = key_fields["hdr.ipv4.protocol"]["value"]
                    srcadr = key_fields["hdr.ipv4.src_addr"]["value"]
                    srcprt = key_fields["ig_md.layer4_srcprt"]["value"]
                    dstadr = key_fields["hdr.ipv4.dst_addr"]["value"]
                    dstprt = key_fields["ig_md.layer4_dstprt"]["value"]
                    ipv4_insp_tx_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    ipv4_insp_tx_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    # entry key has been permuted in order to match ingress flow
                    ntry_key = "%s %s %s %s %s %s" % (
                        aclport_id,
                        prt,
                        inet_ntoa(dstadr),
                        inet_ntoa(srcadr),
                        dstprt,
                        srcprt,
                    )

                    ipv4_inspect_tx_dict[ntry_key] = "%s %s" % (
                        ipv4_insp_tx_pkt_cnt,
                        ipv4_insp_tx_byte_cnt,
                    )
                else:
                    logger.debug(
                        "%s - no ipv4 egress inspect entry in %s"
                        % (self.class_name, tbl_ipv4_inspect_tx_name)
                    )

            if len(ipv4_inspect_rx_dict.keys()) != 0:
                for key in ipv4_inspect_rx_dict.keys():
                    if key not in ipv4_inspect_tx_dict.keys():
                        data = "inspect4_cnt %s %s 0 0 \n" % (
                            key,
                            ipv4_inspect_rx_dict[key],
                        )
                    else:
                        data = "inspect4_cnt %s %s %s \n" % (
                            key,
                            ipv4_inspect_rx_dict[key],
                            ipv4_inspect_tx_dict[key],
                        )
                        ipv4_inspect_tx_dict.pop(key)

                    logger.debug("tx: %s" % data.split(" "))
                    self.file.write(data)
                    self.file.flush()

            if len(ipv4_inspect_tx_dict.keys()) != 0:
                for key in ipv4_inspect_tx_dict.keys():
                    data = "inspect4_cnt %s 0 0 %s \n" % (
                        key,
                        ipv4_inspect_tx_dict[key],
                    )
                    logger.debug("tx: %s" % data.split(" "))
                    self.file.write(data)
                    self.file.flush()

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s and/or %s request problem"
                % (self.class_name, tbl_ipv4_inspect_rx_name, tbl_ipv4_inspect_tx_name)
            )

    def getReadSwitchInspectCounter6(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_ipv6_inspect_rx_name = "%s.ig_ctl_acl_in.tbl_ipv6_insp" % (tbl_global_path)
        tbl_ipv6_inspect_tx_name = "%s.ig_ctl_acl_out.tbl_ipv6_insp" % (tbl_global_path)

        try:
            key_fields = {}
            data_fields = {}
            data = ""
            tbl_ipv6_inspect_rx = self.bfgc.bfrt_info.table_get(
                tbl_ipv6_inspect_rx_name
            )
            tbl_ipv6_inspect_tx = self.bfgc.bfrt_info.table_get(
                tbl_ipv6_inspect_tx_name
            )
            resp_rx = tbl_ipv6_inspect_rx.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )
            resp_tx = tbl_ipv6_inspect_tx.entry_get(
                self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
            )

            ipv6_inspect_rx_dict = {}
            ipv6_inspect_tx_dict = {}

            for d, k in resp_rx:
                key_fields = {}
                data_fields = {}
                data = ""

                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    src_id = key_fields["ig_md.source_id"]["value"]
                    prt = key_fields["hdr.ipv6.next_hdr"]["value"]
                    srcadr = key_fields["hdr.ipv6.src_addr"]["value"]
                    srcprt = key_fields["ig_md.layer4_srcprt"]["value"]
                    dstadr = key_fields["hdr.ipv6.dst_addr"]["value"]
                    dstprt = key_fields["ig_md.layer4_dstprt"]["value"]
                    ipv6_insp_rx_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    ipv6_insp_rx_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    ntry_key = "%s %s %s %s %s %s" % (
                        src_id,
                        prt,
                        inet_ntoa(srcadr),
                        inet_ntoa(dstadr),
                        srcprt,
                        dstprt,
                    )

                    ipv6_inspect_rx_dict[ntry_key] = "%s %s" % (
                        ipv6_insp_rx_pkt_cnt,
                        ipv6_insp_rx_byte_cnt,
                    )

                else:
                    logger.debug(
                        "%s - no ipv6 ingress inspect entry in %s"
                        % (self.class_name, tbl_ipv6_inspect_rx_name)
                    )

            for d, k in resp_tx:
                key_fields = k.to_dict()
                data_fields = d.to_dict()

                if not (key_fields == {}):
                    aclport_id = key_fields["ig_md.aclport_id"]["value"]
                    prt = key_fields["hdr.ipv6.next_hdr"]["value"]
                    srcadr = key_fields["hdr.ipv6.src_addr"]["value"]
                    srcprt = key_fields["ig_md.layer4_srcprt"]["value"]
                    dstadr = key_fields["hdr.ipv6.dst_addr"]["value"]
                    dstprt = key_fields["ig_md.layer4_dstprt"]["value"]
                    ipv6_insp_tx_pkt_cnt = data_fields["$COUNTER_SPEC_PKTS"]
                    ipv6_insp_tx_byte_cnt = data_fields["$COUNTER_SPEC_BYTES"]

                    # entry key has been permuted in order to match ingress flow
                    ntry_key = "%s %s %s %s %s %s" % (
                        aclport_id,
                        prt,
                        inet_ntoa(dstadr),
                        inet_ntoa(srcadr),
                        dstprt,
                        srcprt,
                    )

                    ipv6_inspect_tx_dict[ntry_key] = "%s %s" % (
                        ipv6_insp_tx_pkt_cnt,
                        ipv6_insp_tx_byte_cnt,
                    )
                else:
                    logger.debug(
                        "%s - no ipv6 egress inspect entry in %s"
                        % (self.class_name, tbl_ipv6_inspect_tx_name)
                    )

            if len(ipv6_inspect_rx_dict.keys()) != 0:
                for key in ipv6_inspect_rx_dict.keys():
                    if key not in ipv6_inspect_tx_dict.keys():
                        data = "inspect6_cnt %s %s 0 0 \n" % (
                            key,
                            ipv6_inspect_rx_dict[key],
                        )
                    else:
                        data = "inspect6_cnt %s %s %s \n" % (
                            key,
                            ipv6_inspect_rx_dict[key],
                            ipv6_inspect_tx_dict[key],
                        )
                        ipv6_inspect_tx_dict.pop(key)

                    logger.debug("tx: %s" % data.split(" "))
                    self.file.write(data)
                    self.file.flush()

            if len(ipv6_inspect_tx_dict.keys()) != 0:
                for key in ipv6_inspect_tx_dict.keys():
                    data = "inspect6_cnt %s 0 0 %s \n" % (
                        key,
                        ipv6_inspect_tx_dict[key],
                    )
                    logger.debug("tx: %s" % data.split(" "))
                    self.file.write(data)
                    self.file.flush()

        except:
            logger.warning(_Exception())
            logger.warning(
                "%s - %s and/or %s request problem"
                % (self.class_name, tbl_ipv6_inspect_rx_name, tbl_ipv6_inspect_tx_name)
            )

    def tearDown(self):
        os._exit(0)
        self.die = True
