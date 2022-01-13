from ..bf_gbl_env.cst_env import *

class BfIfSnmpClient(Thread):
    def __init__(self, threadID, name, bfgc, ifmibs_dir,stats_interval,ifindex):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.ifmibs = {}
        self.ifmibs_dir = ifmibs_dir
        self.stats_interval = stats_interval
        self.die=False
        self.snmp_ports=[]
        self.snmp_subifs=[]
        self.ifindex_offset = 1000
        self.ifindex = ifindex

        ## Remove all MIBs to get rid of left-overs from previous runs
        for root, dirs, files in os.walk(self.ifmibs_dir):
            for file in files:
                os.unlink(self.ifmibs_dir + "/" + file)

    def run(self):
        logger.warning("%s - main" % (self.class_name))
        try:
            # re-initialize ifindex file
            shutil.copy("%s.init" % self.ifindex,self.ifindex)
            while not self.die:
                if len(ACTIVE_PORTS.keys())==0:
                    logger.debug("%s - No active ports" % (self.class_name))
                else:
                    for port in ACTIVE_PORTS.keys():
                        if port not in self.snmp_ports:
                            self.addSnmpPort(port,ACTIVE_PORTS[port])
                            self.snmp_ports.append(port)
                            logger.warning("%s - added stats for port %s" % (self.class_name,port))
                    for subif in ACTIVE_SUBIFS.keys():
                        if subif not in self.snmp_subifs:
                                self.addSnmpSubIf(subif)
                                self.snmp_subifs.append(subif)
                                logger.warning("%s - added stats for sub-interface %s" % (self.class_name,subif))
                    for port in self.snmp_ports:
                        if port not in ACTIVE_PORTS.keys():
                            self.deleteSnmpPort(port,0)
                            self.snmp_ports.remove(port)
                            logger.warning("%s - removing stats for port %s" % (self.class_name,port))
                    for subif in self.snmp_subifs:
                        if subif not in ACTIVE_SUBIFS.keys():
                            self.deleteSnmpPort(subif,1)
                            self.snmp_subifs.remove(subif)
                            logger.warning("%s - removing stats for sub-interface %s" % (self.class_name,subif))

                    self.updateStats()

                sleep(self.stats_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - exited with code [%s]" % (self.class_name,_Exception()))
            self.tearDown()

    def addSnmpSubIf(self, port_id):
        if (ACTIVE_SUBIFS[port_id][0] in ACTIVE_PORTS.keys()):
            port_entry = self.bfgc.port_table.entry_get(
                self.bfgc.target,
                [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", ACTIVE_SUBIFS[port_id][0])])],
                {"from-hw": False},
            )
            port_name = next(port_entry)[0].to_dict()["$PORT_NAME"]
            port_massaged = re.sub("/", "_", port_name)
            port_speed = ACTIVE_PORTS[ACTIVE_SUBIFS[port_id][0]]
        else:
            port_name = "%s" % ACTIVE_SUBIFS[port_id][0]
            port_massaged = port_name
            # temporary while determining the best way to retrieve "bundle speed"
            port_speed = 10
        mib_file = "%s/%s.%s" % (self.ifmibs_dir,port_massaged,ACTIVE_SUBIFS[port_id][1])
        ## TODO: we should get the MTU and the ifAlias (interface
        ## description) from the control-plane
        ## Also consider to use logical port names (e.g. "1/0" in the
        ## control-plane and translate them to pyhiscal port ids here)
        self.ifmibs[self.ifindex_offset + port_id] = mib.ifmib(
            mib_file,
            {
                "ifDescr": "%s.%s" % (port_name,ACTIVE_SUBIFS[port_id][1]),
                "ifName": "%s.%s" % (port_name,ACTIVE_SUBIFS[port_id][1]),
                "ifAlias": "Sub-interface %s.%s" % (port_name,ACTIVE_SUBIFS[port_id][1]),
                "ifMtu": 0,
                "speed": port_speed * 1000000000,
            },
        )
        self.updateIndex(WRITE, "%s.%s" % (port_name,ACTIVE_SUBIFS[port_id][1]), port_id)
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
            mib_file = "%s/%s" %(self.ifmibs_dir, port_massaged)
        else:
            port_entry = self.bfgc.port_table.entry_get(
                self.bfgc.target,
                [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", ACTIVE_SUBIFS[subif][0])])],
                {"from-hw": False},
            )
            port_name = next(port_entry)[0].to_dict()["$PORT_NAME"]
            port_massaged = re.sub("/", "_", port_name)
            mib_file = "%s/%s.%s" % (self.ifmibs_dir, port_massaged,ACTIVE_SUBIFS[subif][1])
            self.updateIndex(DELETE, "%s.%s" % (port_name,ACTIVE_SUBIFS[port_id][1], port_id))

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
            #if ACTIVE_PORTS.has_key(port_id):
            if port_id in ACTIVE_PORTS:
                port_entry = self.bfgc.port_table.entry_get(
                    self.bfgc.target,
                    [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                    {"from-hw": False},
                )
                port = next(port_entry)[0].to_dict()
                stat_entry = self.bfgc.stat_table.entry_get(
                    self.bfgc.target,
                    [self.bfgc.stat_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                    {"from-hw": False},
                )
                stat = next(stat_entry)[0].to_dict()
                ifTable.update(port, stat)
                logger.debug("Update Port SNMP stats for %s with %s" % (port_id,stat))
            else:
                port = {}
                port['$PORT_ENABLE']=True
                port['$PORT_UP']=True
                stat = self.createSubIfStatsFromCounters(port_id - self.ifindex_offset)
                ifTable.update(port, stat)
                logger.debug("Update Sub-interface SNMP stats for %s" % (port_id - self.ifindex_offset))

    def createSubIfStatsFromCounters(self, port_id):
        try:
            stat = {}
            logger.debug(" SUBIF_COUNTERS[port_id]: %s" % SUBIF_COUNTERS[port_id])
            #if SUBIF_COUNTERS.has_key(port_id):
            if port_id in SUBIF_COUNTERS:
                stat['$OctetsReceivedinGoodFrames']=SUBIF_COUNTERS[port_id][1]
                stat['$FramesReceivedOK']=SUBIF_COUNTERS[port_id][0]
                stat['$FramesDroppedBufferFull']=0
                stat['$FrameswithanyError']=0
                stat['$OctetsTransmittedwithouterror']=SUBIF_COUNTERS[port_id][3]
                stat['$FramesTransmittedOK']=SUBIF_COUNTERS[port_id][2]
                stat['$FramesTransmittedwithError']=0
                stat['$FramesReceivedwithMulticastAddresses']=0
                stat['$FramesReceivedwithBroadcastAddresses']=0
                stat['$FramesTransmittedMulticast']=0
                stat['$FramesTransmittedBroadcast']=0
                return stat
            else:
                return None

        except Exception as e:
            logger.warning("Error cleaning up: {}".format(e))

    def updateIndex(self,op_type, mib_file,mib_ifindex):
        if op_type == WRITE:
            # copy current ifindex file
            shutil.copy(self.ifindex,"%s.prev" % self.ifindex)
            with open(self.ifindex, "a+") as output:
                output.write("%s %s\n" % (mib_file,self.ifindex_offset + mib_ifindex))
                #output.close()
        if op_type == DELETE:
            logger.warning("updateIndex:DELETE")
            shutil.copy(self.ifindex,"%s.prev" % self.ifindex)
            with open("%s.prev" % self.ifindex, "r") as input:
                with open(self.ifindex, "w") as output:
                    for line in input:
                        if line.strip("\n") != "%s %s" % (mib_file,self.ifindex_offset + mib_ifindex):
                            output.write(line)
                    #output.close()
                #input.close()

        # reload if-snmp-agent with new ifindex with systemd
        # TBD


    def tearDown(self):
        self.die=True
