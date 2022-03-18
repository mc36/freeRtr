from ..bf_gbl_env.var_env import *

## Mappings of values of the $SPEED field in the
## $PORT table to bps. Used in the ifMIB to set
## the ifSpeed/ifHighSpeed elements.
if_speed = {
    'BF_SPEED_NONE':                         0,
    'BF_SPEED_1G':                  1000000000,
    'BF_SPEED_10G':                10000000000,
    'BF_SPEED_25G':                25000000000,
    'BF_SPEED_40G':                40000000000,
    'BF_SPEED_40G_NB':             40000000000,
    'BF_SPEED_40G_NON_BREAKABLE':  40000000000,
    'BF_SPEED_50G':                50000000000,
    'BF_SPEED_100G':              100000000000,
    'BF_SPEED_200G':              200000000000,
    'BF_SPEED_400G':              400000000000
}

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


class BfPorts(Thread):
    def __init__(
            self,
            threadID,
            name,
            bfgc,
            sck_file,
            pipe_name,
            status_interval,
            counter_interval_multiplier,
            snmp_enable,
            ifmibs_dir,
            ifindex_file
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.status_interval = status_interval
        self.counter_mp = counter_interval_multiplier
        self.pipe_name = pipe_name
        ## Key: physical port ID
        ## Value: the port's entry in the $PORT table
        self.ifs = {}
        ## Key: pseudo port ID
        ## Value: dictionary holding the physical port ID and VLAN ID
        self.subifs = {}
        ##
        self.snmp_enable = snmp_enable
        self.ifmibs_dir = ifmibs_dir
        self.ifindex_file = ifindex_file
        self.ifindex_pseudo_port_offset = 1000
        ## The mib dicts are indexed by the port/pseudo-port
        ## IDs. Currently, these IDs use the same number space and the
        ## mibs could be kept in a single table. The code here is
        ## designed to also work if separate number spaces are used
        ## for the physical and pseudo port IDs.
        self.ifmibs = {}
        self.subifmibs = {}

        ig_path = "%s.ig_ctl" % self.pipe_name
        eg_path = "%s.eg_ctl" % self.pipe_name

        ## VLAN table, mapping a pseudo-port to its physical port and
        ## VLAN ID
        tbl_name_vlan_in = "%s.ig_ctl_vlan_in.tbl_vlan_in" % (ig_path)
        self.tbl_vlan_in = self.bfgc.bfrt_info.table_get(tbl_name_vlan_in)

        ## Indirect counters fo pseudo-ports
        cnt_name_stats_in = "%s.ig_ctl_vlan_in.stats" % (ig_path)
        self.cnt_stats_in = self.bfgc.bfrt_info.table_get(cnt_name_stats_in)
        cnt_name_stats_out = "%s.eg_ctl_vlan_out.stats" % (eg_path)
        self.cnt_stats_out = self.bfgc.bfrt_info.table_get(cnt_name_stats_out)

        if self.snmp_enable:
            ## Remove all MIBs to get rid of left-overs from previous runs
            for root, dirs, files in os.walk(self.ifmibs_dir):
                for file in files:
                    os.unlink(self.ifmibs_dir + "/" + file)

    ## Wrappers for the low-level table access functions
    def entryGet(self, table, keys = None, fromHw = True):
        if keys is not None:
            key_list =  [ table.make_key(
                list(map(lambda key: gc.KeyTuple(**key), keys))) ]
        else:
            key_list = []
        return table.entry_get(self.bfgc.target,
                               key_list,
                               { "from_hw": fromHw },
                               p4_name = self.bfgc.p4_name)

    def entryGetDict(self, table, keys = None, fromHw = True):
        resp = self.entryGet(table, keys, fromHw)
        return next(resp)[0].to_dict()

    def addSnmpIf(self, port_id, port_data):
        port_name = port_data["$PORT_NAME"]
        port_massaged = re.sub("/", "_", port_name)
        mib_file = "%s/%s" % (self.ifmibs_dir, port_massaged)
        ## TODO: we should get the the ifAlias (interface description)
        ## from the control-plane.
        ## TODO: ifDescr should be the name of the interface as it
        ## appears in the freerouter configuration. This is currently
        ## not sent by the control-plane. With that change, we will
        ## also need to update the persistent ifindex table whenever a
        ## port is added or removed.
        self.ifmibs[port_id] = mib.ifmib(
            mib_file,
            {
                "ifDescr": port_name,
                "ifName": port_name,
                "ifAlias": "Port {0:s}".format(port_name),
                "ifMtu": port_data['$TX_MTU'],
                "speed": if_speed[port_data['$SPEED']]
            },
        )
        logger.debug("Port[%s] added IF MIB %s" % (port_id, mib_file))

    def addSubIfToIfIndex(self, ifDescr, port_id):
        ifindex = port_id + self.ifindex_pseudo_port_offset
        logger.debug("Adding port %s, ifindex %s to %s"
                     % (ifDescr, ifindex, self.ifindex_file))
        with open(self.ifindex_file, "a+") as output:
            output.write("%s %s\n" % (ifDescr, ifindex))

    def removeSubIfFromIfIndex(port_id):
        ifindex = port_id + self.ifindex_pseudo_port_offset
        logger.debug("Removing port %s, ifindex %s from %s"
                     % (ifDescr, ifindex, self.ifindex_file))
        ifindex_tmp = "%s.prev" % self.ifindex_file
        shutil.copy(self.ifindex, ifindex_tmp)
        with open(ifindex_tmp, "r") as input:
            with open(self.ifindex, "w") as output:
                for line in input:
                    if ifindex not in line.strip("\n"):
                        output.write(line)

    def addSnmpSubIf(self, port_id, port_data):
        phys_port_id = port_data['phys_port_id']
        vlan_id = port_data['vlan_id']
        if phys_port_id in self.ifs:
            phys_port_data = self.ifs[phys_port_id]
            port_name = phys_port_data["$PORT_NAME"]
            port_massaged = re.sub("/", "_", port_name)
            port_speed = if_speed[phys_port_data['$SPEED']]
            mtu = phys_port_data['$TX_MTU']
        else:
            ## Bundle interface (?)
            ## TODO: find a more reasonable name
            port_name = "%s" % phys_port_id
            port_massaged = port_name
            ## TODO: set speed and mtu properly
            port_speed = 10
            mtu = 0
        mib_file = "%s/%s.%s" % (
            self.ifmibs_dir,
            port_massaged,
            vlan_id
        )
        ## TODO: we should get the ifAlias (interface description)
        ## from the control-plane,
        ifDescr = "%s.%s" % (port_name, vlan_id)
        self.subifmibs[port_id] = mib.ifmib(
            mib_file,
            {
                "ifDescr": ifDescr,
                "ifName": ifDescr,
                "ifAlias": "Sub-interface %s" % ifDescr,
                "ifMtu": mtu,
                "speed": port_speed
            },
        )
        self.addSubIfToIfIndex(ifDescr, port_id)
        logger.debug("Port[%s] added IF MIB %s" % (port_id, mib_file))

    def operStateChange(self, port_id, state_up):
        state = (port_id, 1 if state_up else 0)
        self.file.write("state %s %s\n" % state)
        self.file.flush()
        logger.warning("tx: ['state','%s','%s','\\n']" % state)
        logger.warning(
            "%s - port [%s] operational state changed to %s"
            % (self.class_name, port_id, "UP" if state_up else "DOWN")
        )

    def getIfs(self):
        ports = self.entryGet(self.bfgc.port_table)
        old_ifs = self.ifs
        self.ifs = {}
        for d, k in ports:
            keys = k.to_dict()
            port_data = d.to_dict()
            port_id = keys["$DEV_PORT"]["value"]
            self.ifs[port_id] = port_data
            ## Detect change in oper status
            new_oper_state = port_data["$PORT_UP"]
            if port_id not in old_ifs or new_oper_state != old_ifs[port_id]["$PORT_UP"]:
                self.operStateChange(port_id, new_oper_state)

        if self.snmp_enable:
            for port_id in set(self.ifs) - set(self.ifmibs):
                self.addSnmpIf(port_id, self.ifs[port_id])
            for port_id in set(self.ifmibs) - set(self.ifs):
                del self.ifmibs[port_id]

    def getSubIfs(self):
        vlans = self.entryGet(self.tbl_vlan_in)
        for d, k in vlans:
            keys = k.to_dict()
            data = d.to_dict()
            port_id = data["src"]
            port_data = {
                'phys_port_id': keys["ig_md.ingress_id"]["value"],
                'vlan_id': keys["hdr.vlan.vid"]["value"]
            }
            self.subifs[port_id] = port_data
        if self.snmp_enable:
            for port_id in set(self.subifs) - set(self.subifmibs):
                self.addSnmpSubIf(port_id, self.subifs[port_id])
            for port_id in set(self.subifmibs) - set(self.subifs):
                del self.subifmibs[port_id]
                self.removeSubIfFromIfIndex(port_id)

    def getIfCounters(self):
        for port_id, port_data in self.ifs.items():
            stat = self.entryGetDict(self.bfgc.stat_table,
                                     [ { 'name': '$DEV_PORT', 'value': port_id } ])
            result = (
                port_id,
                stat["$FramesReceivedAll"],
                stat["$OctetsReceived"],
                stat["$FramesTransmittedAll"],
                stat["$OctetsTransmittedTotal"],
                stat["$FrameswithanyError"],
            )

            logger.debug(
                "tx: ['counter', '%s', '%s', '%s', '%s', '%s', '%s', '\\n']" % result)
            self.file.write("counter %s %s %s %s %s %s\n" % result)
            self.file.flush()

            if self.snmp_enable:
                self.ifmibs[port_id].update(port_data, stat)

    def getSubIfCounters(self):
        for port_id, port_data in self.subifs.items():
            logger.debug("%s - reading counter[%s]" % (self.class_name, port_id))
            stats_in = self.entryGetDict(self.cnt_stats_in,
                                         [ { 'name': '$COUNTER_INDEX', 'value': port_id }])
            logger.debug("INGRESS STATS FOR SUBIF[%s]=%s" % (port_id, stats_in))
            stats_out = self.entryGetDict(self.cnt_stats_out,
                                          [ { 'name': '$COUNTER_INDEX', 'value': port_id }])
            logger.debug("EGRESS STATS FOR SUBIF[%s]=%s" % (port_id, stats_out))
            result = (
                port_id,
                stats_in["$COUNTER_SPEC_PKTS"],
                stats_in["$COUNTER_SPEC_BYTES"],
                stats_out["$COUNTER_SPEC_PKTS"],
                stats_out["$COUNTER_SPEC_BYTES"],
            )

            logger.debug(
                "tx: ['counter', '%s', '%s', '%s', '%s', '%s', '\\n']"
                % result)
            self.file.write(
                "counter %s %s %s %s %s\n" % result)
            self.file.flush()

            if self.snmp_enable:
                ## If a physical port is admin down, it is removed
                ## from the $PORT table by rare/api/state.py and hence
                ## also doesn't exist in the ifs table.  However, the
                ## sub-interfaces do exist because the VLAN table gets
                ## populated by rare/api/portvlan.py anyway. In this
                ## situation, the MIB contains a row for the
                ## sub-interface but not the physical interface and we
                ## don't have access to the $PORT entry. For now, we
                ## create a fake entry but that entire logic needs to
                ## be fixed.
                phys_port_id = port_data['phys_port_id']
                if phys_port_id in self.ifs:
                    phys_port_data = self.ifs[phys_port_id]
                else:
                    phys_port_data = {
                        '$PORT_ENABLE': False,
                        '$PORT_UP': False
                    }
                ## Create an artificial stats table to pass to the
                ## ifTable update() method
                stat = {
                    "$OctetsReceivedinGoodFrames":    stats_in["$COUNTER_SPEC_BYTES"],
                    "$FramesReceivedOK":              stats_in["$COUNTER_SPEC_PKTS"],
                    "$FramesDroppedBufferFull":       0,
                    "$FrameswithanyError":            0,
                    "$OctetsTransmittedwithouterror": stats_out["$COUNTER_SPEC_BYTES"],
                    "$FramesTransmittedOK":           stats_out["$COUNTER_SPEC_PKTS"],
                    "$FramesTransmittedwithError":    0,
                    "$FramesReceivedwithMulticastAddresses": 0,
                    "$FramesReceivedwithBroadcastAddresses": 0,
                    "$FramesTransmittedMulticast":    0,
                    "$FramesTransmittedBroadcast":    0
                }
                self.subifmibs[port_id].update(phys_port_data, stat)

    def sendPortInfoToCP(self):
        ## Determine the mapping from port names in the form
        ## "connector/channel" to physical port IDs.  It is provided
        ## directly by the $PORT_STR_INFO table. However, this table
        ## (like all "internal" tables) can only be queried for
        ## specific keys, i.e. traversal of all entries by providing
        ## an empty list of keys is not supported. As a workaround, we
        ## use the $PORT_HDL_INFO table instead and simply iterate
        ## over all connector/channel pairs and pick out those that
        ## are actually present. Scanning for connectors from 1 to 65
        ## should cover all current Tofino models.
        for conn in range(1, 65):
            for chnl in range(0,4):
                try:
                    resp = self.bfgc.port_hdl_info_table.entry_get(
                        self.bfgc.target, [
                            self.bfgc.port_hdl_info_table.make_key([
                                gc.KeyTuple("$CONN_ID", conn),
                                gc.KeyTuple("$CHNL_ID", chnl)
                            ])
                        ],
                        {"from_hw": False},
                        p4_name=self.bfgc.p4_name
                    )
                    dev_port = next(resp)[0].to_dict()['$DEV_PORT']
                except Exception as e:
                    ## Skip non-existant entries.  TODO: make sure
                    ## this is the only error to be expected here
                    pass
                else:
                    data = "portname %s frontpanel-%s/%s \n" % (dev_port, conn, chnl)
                    logger.warning("tx: %s" % data.split(" "))
                    self.file.write(data)
        self.file.write("dynrange 512 1023 \n")
        self.file.flush()

    def run(self):
        try:
            logger.warning("%s - main" % (self.class_name))
            if self.snmp_enable:
                ## Reset the ifindex file
                shutil.copy("%s.init" % self.ifindex_file, self.ifindex_file)
            self.sendPortInfoToCP()
            i = 0
            while not self.die:
                i += 1
                self.getIfs()
                if i % self.counter_mp == 0:
                    self.getSubIfs()
                    logger.debug("%s - loop" % (self.class_name))
                    if len(self.ifs.keys()) == 0:
                        logger.debug("%s - No active interfaces" % (self.class_name))
                    else:
                        logger.debug(
                            "%s - IFS %s"
                            % (self.class_name, self.ifs.keys())
                        )
                        self.getIfCounters()
                        if len(self.subifs.keys()) == 0:
                            logger.debug("%s - No active sub-interfaces" % (self.class_name))
                        else:
                            logger.debug(
                                "%s - SUBIFS %s"
                                % (self.class_name, self.subifs.keys())
                            )
                            self.getSubIfCounters()
                sleep(self.status_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning(
                "%s - exited with code [%s]" % (self.class_name, _Exception())
            )
            self.tearDown()

    def tearDown(self):
        os._exit(0)
        self.die = True
