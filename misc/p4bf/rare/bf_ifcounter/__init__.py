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


class BfIfCounter(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        pipe_name,
        subif_counter_interval,
    ):
        # def __init__(self, threadID, name, bfgc, pipe_name,subif_counter_interval,):
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
                    self.getReadSwitchIfCounter()
                sleep(self.subif_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning(
                "%s - exited with code [%s]" % (self.class_name, _Exception())
            )
            self.tearDown()

    def getReadSwitchIfCounter(self):
        # for all subif
        for port_id in self.active_ports.keys():
            port_entry = self.bfgc.port_table.entry_get(
                self.bfgc.target,
                [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                {"from_hw": True},
                p4_name=self.bfgc.p4_name,
            )
            port = next(port_entry)[0].to_dict()

            stat_entry = self.bfgc.stat_table.entry_get(
                self.bfgc.target,
                [self.bfgc.stat_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                {"from_hw": True},
                p4_name=self.bfgc.p4_name,
            )

            stat = next(stat_entry)[0].to_dict()

            rx = "$FramesReceivedAll[%s] - $OctetsReceived[%s]" % (
                stat["$FramesReceivedAll"],
                stat["$OctetsReceived"],
            )

            tx = "$FramesTransmittedAll[%s] - $OctetsTransmittedTotal[%s]" % (
                stat["$FramesTransmittedAll"],
                stat["$OctetsTransmittedTotal"],
            )

            err = "$FrameswithanyError[%s]" % (stat["$FrameswithanyError"])

            logger.debug(
                "tx: ['counter','%s','%s','%s', '%s', '%s' ,'%s'\\n']"
                % (
                    port_id,
                    stat["$FramesReceivedAll"],
                    stat["$OctetsReceived"],
                    stat["$FramesTransmittedAll"],
                    stat["$OctetsTransmittedTotal"],
                    stat["$FrameswithanyError"],
                )
            )

            self.file.write(
                "counter %s %s %s %s %s %s\n"
                % (
                    port_id,
                    stat["$FramesReceivedAll"],
                    stat["$OctetsReceived"],
                    stat["$FramesTransmittedAll"],
                    stat["$OctetsTransmittedTotal"],
                    stat["$FrameswithanyError"],
                )
            )
            self.file.flush()

    def tearDown(self):
        os._exit(0)
        self.die = True
