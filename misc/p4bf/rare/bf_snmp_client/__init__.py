from ..bf_gbl_env.cst_env import *


class BfIfSnmpClient(Thread):
    def __init__(self, threadID, name, bfgc, ifmibs_dir, stats_interval, ifindex):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.ifmibs = {}
        self.ifmibs_dir = ifmibs_dir
        self.stats_interval = stats_interval
        self.die = False
        self.snmp_ports = []
        self.snmp_subifs = []
        self.ifindex_offset = 1000
        self.ifindex = ifindex
        self.active_ports = {}
        self.active_subifs = {}
        self.subif_counters = {}

        ## Remove all MIBs to get rid of left-overs from previous runs
        for root, dirs, files in os.walk(self.ifmibs_dir):
            for file in files:
                os.unlink(self.ifmibs_dir + "/" + file)

    def run(self):
        logger.warning("%s - main" % (self.class_name))
        try:
            # re-initialize ifindex file
            shutil.copy("%s.init" % self.ifindex, self.ifindex)
            while not self.die:
                self.active_ports = self.getAllActivePorts()
                self.active_subifs = self.getAllActiveSubInterfaces()
                self.getReadSwitchSubIfCounter()

                if len(self.active_ports.keys()) == 0:
                    logger.debug("%s - No active ports" % (self.class_name))
                else:
                    for port in self.active_ports.keys():
                        if port not in self.snmp_ports:
                            self.addSnmpPort(port, self.active_ports[port])
                            self.snmp_ports.append(port)
                            logger.warning(
                                "%s - added stats for port %s" % (self.class_name, port)
                            )
                    for subif in self.active_subifs.keys():
                        if subif not in self.snmp_subifs:
                            self.addSnmpSubIf(subif)
                            self.snmp_subifs.append(subif)
                            logger.warning(
                                "%s - added stats for sub-interface %s"
                                % (self.class_name, subif)
                            )
                    for port in self.snmp_ports:
                        if port not in self.active_ports.keys():
                            self.deleteSnmpPort(port, 0)
                            self.snmp_ports.remove(port)
                            logger.warning(
                                "%s - removing stats for port %s"
                                % (self.class_name, port)
                            )
                    for subif in self.snmp_subifs:
                        if subif not in self.active_subifs.keys():
                            self.deleteSnmpPort(subif, 1)
                            self.snmp_subifs.remove(subif)
                            logger.warning(
                                "%s - removing stats for sub-interface %s"
                                % (self.class_name, subif)
                            )

                    self.updateStats()

                sleep(self.stats_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning(
                "%s - exited with code [%s]" % (self.class_name, _Exception())
            )
            self.tearDown()

    def addSnmpSubIf(self, port_id):
        if self.active_subifs[port_id][0] in self.active_ports.keys():
            port_entry = self.bfgc.port_table.entry_get(
                self.bfgc.target,
                [
                    self.bfgc.port_table.make_key(
                        [gc.KeyTuple("$DEV_PORT", self.active_subifs[port_id][0])]
                    )
                ],
                {"from-hw": False},
            )
            port_name = next(port_entry)[0].to_dict()["$PORT_NAME"]
            port_massaged = re.sub("/", "_", port_name)
            port_speed = self.active_ports[self.active_subifs[port_id][0]]
        else:
            port_name = "%s" % self.active_subifs[port_id][0]
            port_massaged = port_name
            # temporary while determining the best way to retrieve "bundle speed"
            port_speed = 10
        mib_file = "%s/%s.%s" % (
            self.ifmibs_dir,
            port_massaged,
            self.active_subifs[port_id][1],
        )
        ## TODO: we should get the MTU and the ifAlias (interface
        ## description) from the control-plane
        ## Also consider to use logical port names (e.g. "1/0" in the
        ## control-plane and translate them to pyhiscal port ids here)
        self.ifmibs[self.ifindex_offset + port_id] = mib.ifmib(
            mib_file,
            {
                "ifDescr": "%s.%s" % (port_name, self.active_subifs[port_id][1]),
                "ifName": "%s.%s" % (port_name, self.active_subifs[port_id][1]),
                "ifAlias": "Sub-interface %s.%s"
                % (port_name, self.active_subifs[port_id][1]),
                "ifMtu": 0,
                "speed": port_speed * 1000000000,
            },
        )
        self.updateIndex(
            WRITE, "%s.%s" % (port_name, self.active_subifs[port_id][1]), port_id
        )
        logger.debug("Port[%s] added IF MIB", mib_file)

    def addSnmpPort(self, port_id, port_speed):
        port_entry = self.bfgc.port_table.entry_get(
            self.bfgc.target,
            [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
            {"from-hw": False},
        )
        port_name = next(port_entry)[0].to_dict()["$PORT_NAME"]
        port_massaged = re.sub("/", "_", port_name)
        mib_file = "%s/%s" % (self.ifmibs_dir, port_massaged)
        ## TODO: we should get the MTU and the ifAlias (interface
        ## description) from the control-plane
        ## Also consider to use logical port names (e.g. "1/0" in the
        ## control-plane and translate them to pyhiscal port ids here)
        self.ifmibs[port_id] = mib.ifmib(
            mib_file,
            {
                "ifDescr": port_name,
                "ifName": port_name,
                "ifAlias": "Port {0:s}".format(port_name),
                "ifMtu": 0,
                "speed": port_speed * 1000000000,
            },
        )
        logger.debug("Port[%s] added IF MIB", mib_file)

    def deleteSnmpPort(self, port_id, if_type):
        if if_type == 0:
            port_entry = self.bfgc.port_table.entry_get(
                self.bfgc.target,
                [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                {"from-hw": False},
            )
            port_name = next(port_entry)[0].to_dict()["$PORT_NAME"]
            port_massaged = re.sub("/", "_", port_name)
            mib_file = "%s/%s" % (self.ifmibs_dir, port_massaged)
        else:
            port_entry = self.bfgc.port_table.entry_get(
                self.bfgc.target,
                [
                    self.bfgc.port_table.make_key(
                        [gc.KeyTuple("$DEV_PORT", self.active_subifs[subif][0])]
                    )
                ],
                {"from-hw": False},
            )
            port_name = next(port_entry)[0].to_dict()["$PORT_NAME"]
            port_massaged = re.sub("/", "_", port_name)
            mib_file = "%s/%s.%s" % (
                self.ifmibs_dir,
                port_massaged,
                self.active_subifs[subif][1],
            )
            self.updateIndex(
                DELETE, "%s.%s" % (port_name, self.active_subifs[port_id][1], port_id)
            )

        # try to unlink file
        # if file exist
        try:
            if os.path.exists(mib_file):
                os.unlink(mib_file)
            # if not do nothing
            logger.debug("Port[%s] removed", mib_file)

        except Exception as e:
            logger.debug("Error removing %s" % mib_file)
            logger.debug("Exception: %s" % (e))

    def updateStats(self):
        for port_id, ifTable in self.ifmibs.items():
            # if self.active_ports.has_key(port_id):
            if port_id in self.active_ports:
                port_entry = self.bfgc.port_table.entry_get(
                    self.bfgc.target,
                    [
                        self.bfgc.port_table.make_key(
                            [gc.KeyTuple("$DEV_PORT", port_id)]
                        )
                    ],
                    {"from-hw": False},
                )
                port = next(port_entry)[0].to_dict()
                stat_entry = self.bfgc.stat_table.entry_get(
                    self.bfgc.target,
                    [
                        self.bfgc.stat_table.make_key(
                            [gc.KeyTuple("$DEV_PORT", port_id)]
                        )
                    ],
                    {"from-hw": False},
                )
                stat = next(stat_entry)[0].to_dict()
                ifTable.update(port, stat)
                logger.debug("Update Port SNMP stats for %s with %s" % (port_id, stat))
            else:
                port = {}
                port["$PORT_ENABLE"] = True
                port["$PORT_UP"] = True
                stat = self.createSubIfStatsFromCounters(port_id - self.ifindex_offset)
                ifTable.update(port, stat)
                logger.debug(
                    "Update Sub-interface SNMP stats for %s"
                    % (port_id - self.ifindex_offset)
                )

    def createSubIfStatsFromCounters(self, port_id):
        try:
            stat = {}
            logger.debug(
                " self.subif_counters[port_id]: %s" % self.subif_counters[port_id]
            )
            # if self.subif_counters.has_key(port_id):
            if port_id in self.subif_counters:
                stat["$OctetsReceivedinGoodFrames"] = self.subif_counters[port_id][1]
                stat["$FramesReceivedOK"] = self.subif_counters[port_id][0]
                stat["$FramesDroppedBufferFull"] = 0
                stat["$FrameswithanyError"] = 0
                stat["$OctetsTransmittedwithouterror"] = self.subif_counters[port_id][3]
                stat["$FramesTransmittedOK"] = self.subif_counters[port_id][2]
                stat["$FramesTransmittedwithError"] = 0
                stat["$FramesReceivedwithMulticastAddresses"] = 0
                stat["$FramesReceivedwithBroadcastAddresses"] = 0
                stat["$FramesTransmittedMulticast"] = 0
                stat["$FramesTransmittedBroadcast"] = 0
                return stat
            else:
                return None

        except Exception as e:
            logger.warning("Error cleaning up: {}".format(e))

    def updateIndex(self, op_type, mib_file, mib_ifindex):
        if op_type == WRITE:
            # copy current ifindex file
            shutil.copy(self.ifindex, "%s.prev" % self.ifindex)
            with open(self.ifindex, "a+") as output:
                output.write("%s %s\n" % (mib_file, self.ifindex_offset + mib_ifindex))
                # output.close()
        if op_type == DELETE:
            logger.warning("updateIndex:DELETE")
            shutil.copy(self.ifindex, "%s.prev" % self.ifindex)
            with open("%s.prev" % self.ifindex, "r") as input:
                with open(self.ifindex, "w") as output:
                    for line in input:
                        if line.strip("\n") != "%s %s" % (
                            mib_file,
                            self.ifindex_offset + mib_ifindex,
                        ):
                            output.write(line)
                    # output.close()
                # input.close()

        # reload if-snmp-agent with new ifindex with systemd
        # TBD

    def getAllActivePorts(self):
        resp = self.bfgc.port_table.entry_get(
            self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
        )
        self.active_ports = {}
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            self.active_ports[key_fields["$DEV_PORT"]["value"]] = data_fields[
                "$PORT_NAME"
            ]
        return self.active_ports

    def getAllActiveSubInterfaces(self):
        tbl_global_path = "%s.ig_ctl" % self.pipe_name
        tbl_name_vlan_in = "%s.ig_ctl_vlan_in.tbl_vlan_in" % (tbl_global_path)
        tbl_vlan_in = self.bfgc.bfrt_info.table_get(tbl_name_vlan_in)
        resp = tbl_vlan_in.entry_get(
            self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
        )
        active_subifs = {}
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            active_subifs[data_fields["src"]] = [
                key_fields["ig_md.ingress_id"]["value"],
                key_fields["hdr.vlan.vid"]["value"],
            ]
        return active_subifs

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

        tbl_stats_in.operations_execute(
            self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
        )
        tbl_stats_out.operations_execute(
            self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
        )
        tbl_stats_pkt_out.operations_execute(
            self.bfgc.target, "Sync", p4_name=self.bfgc.p4_name
        )

        resp = tbl_vlan_in.entry_get(
            self.bfgc.target, [], {"from_hw": False}, p4_name=self.bfgc.p4_name
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
            logger.debug("self.subif_counters[%s]=%s" % (key, self.subif_counters[key]))

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
                {"from-hw": False},
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
                {"from-hw": False},
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
                {"from-hw": False},
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

            # self.file.write("counter %s %s %s %s %s\n"
            #                    % (counter_id,
            #                    stats_in["$COUNTER_SPEC_PKTS"] - stats_pkt_out["$COUNTER_SPEC_PKTS"],
            #                    stats_in["$COUNTER_SPEC_BYTES"] - stats_pkt_out["$COUNTER_SPEC_BYTES"],
            #                    stats_out["$COUNTER_SPEC_PKTS"] + stats_pkt_out["$COUNTER_SPEC_PKTS"],
            #                    stats_out["$COUNTER_SPEC_BYTES"] + stats_pkt_out["$COUNTER_SPEC_BYTES"]))
            # self.file.flush()

    def tearDown(self):
        self.die = True
