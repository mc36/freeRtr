from ..bf_gbl_env.var_env import *

PORTS_OPER_STATUS = {}


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


from .bf_mac import tnaparser, getmacaddr


class BfIfStatus(Thread):
    def __init__(
        self,
        threadID,
        name,
        bfgc,
        sck_file,
        oper_status_interval,
    ):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.oper_status_interval = oper_status_interval
        self.all_ports = []

    from .bf_mac import TNAPort, tnaparser, getmacaddr

    def sendPortInfoToCP(self):
        for p in self.all_ports:
            data = "portname %s %s \n" % (p.dp, p.port)
            logger.warning("tx: %s" % data.split(" "))
            self.file.write(data)
            self.file.flush()

    def getAllActivePorts(self):
        resp = self.bfgc.port_table.entry_get(
            self.bfgc.target, [], {"from_hw": True}, p4_name=self.bfgc.p4_name
        )
        ACTIVE_PORTS = {}
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            ACTIVE_PORTS[key_fields["$DEV_PORT"]["value"]] = data_fields["$PORT_NAME"]
        return ACTIVE_PORTS

    def run(self):
        try:

            self.all_ports = tnaparser()
            self.sendPortInfoToCP()

            logger.warning("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - loop" % (self.class_name))
                logger.debug("%s - %s" % (self.class_name, self.getAllActivePorts()))
                self.getAllActivePorts()

                if len(self.getAllActivePorts().keys()) == 0:
                    logger.debug("%s - No active ports" % (self.class_name))
                else:
                    self.getActiveSwitchOperStatus()

                sleep(self.oper_status_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning(
                "%s - exited with code [%s]" % (self.class_name, _Exception())
            )
            self.tearDown()

    def getActiveSwitchOperStatus(self):
        ACTIVE_PORTS = self.getAllActivePorts()
        for port_id in ACTIVE_PORTS.keys():
            key_list = [
                self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])
            ]
            data_list = self.bfgc.port_table.make_data([gc.DataTuple("$PORT_UP")])
            port_entry = self.bfgc.port_table.entry_get(
                self.bfgc.target,
                key_list,
                {"from_hw": True},
                data_list,
                p4_name=self.bfgc.p4_name,
            )
            port = next(port_entry)[0].to_dict()
            if port_id in PORTS_OPER_STATUS:
                # notify control plane if needed
                if PORTS_OPER_STATUS[port_id] == port["$PORT_UP"]:
                    logger.debug(
                        "%s - PORTS_OPER_STATUS[%s] no state change -> OPR: %s"
                        % (self.class_name, port_id, port["$PORT_UP"])
                    )
                    continue
                elif (PORTS_OPER_STATUS[port_id] == True) and (
                    port["$PORT_UP"] == False
                ):
                    self.file.write("state %s %s\n" % (port_id, 0))
                    self.file.flush()
                    logger.warning("tx: ['state','%s','0','\\n']" % port_id)
                    PORTS_OPER_STATUS[port_id] = False
                    logger.debug(
                        "%s - PORTS_OPER_STATUS[%s] state change to DOWN"
                        % (self.class_name, port_id)
                    )
                elif (PORTS_OPER_STATUS[port_id] == False) and (
                    port["$PORT_UP"] == True
                ):
                    self.file.write("state %s %s\n" % (port_id, 1))
                    self.file.flush()
                    logger.warning("tx: ['state','%s','1','\\n']" % port_id)
                    PORTS_OPER_STATUS[port_id] = True
                    logger.debug(
                        "%s - PORTS_OPER_STATUS[%s] state change to UP"
                        % (self.class_name, port_id)
                    )

            else:
                logger.warning(
                    "%s - PORTS_OPER_STATUS[%s] does not exist, adding it -> OPR: %s "
                    % (self.class_name, port_id, port["$PORT_UP"])
                )
                PORTS_OPER_STATUS[port_id] = port["$PORT_UP"]

    def tearDown(self):
        os._exit(0)
        self.die = True
