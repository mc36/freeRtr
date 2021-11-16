#! /usr/bin/env python3

###############################################################################
#
# Copyright 2019-present GEANT RARE project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed On an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
###############################################################################

from threading import Thread
import argparse, grpc, os, sys, socket, logging, mib, re, linecache, shutil, inspect
from time import sleep

SDE = os.environ.get("SDE", "~/bf-sde-9.7.0")
SDE_INSTALL = os.environ.get("SDE_INSTALL", SDE + "/install")
BF_RUNTIME_LIB = SDE_INSTALL + "/lib/python3.8/site-packages/tofino/"
BSP_FILE_PATH = SDE_INSTALL + "/lib/libpltfm_mgr.so"

# set our lib path
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "./", BF_RUNTIME_LIB))
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "./", BF_RUNTIME_LIB+"bfrt_grpc"))
import bfrt_grpc.client as gc
import bfrt_grpc.bfruntime_pb2 as bfrt_pb2

PROGRAM_NAME = os.path.basename(sys.argv[0])

log_level = logging.WARNING

logger = logging.getLogger(PROGRAM_NAME)
if not len(logger.handlers):
    logger.addHandler(logging.StreamHandler())
    logger.setLevel(log_level)

try:
    from sal_services_pb2 import (
        UNDEFINED_SPEED,
        SPEED_NONE,
        SPEED_1G,
        SPEED_2P5G,
        SPEED_5G,
        SPEED_10G,
        SPEED_25G,
        SPEED_40G,
        SPEED_50G,
        SPEED_100G,
    )
    from sal_services_pb2 import UNDEFINED_FEC, FEC_NONE, FEC_FC, FEC_RS
    from sal_services_pb2 import UNDEFINED_FC, FC_NONE, FC_PAUSE, FC_PFC
    from sal_services_pb2 import UNDEFINED_AN, AN_OFF, AN_ON
    from salgrpcclient import SalGrpcClient, SAL_PORT_ID
except ImportError:
    logger.warning("sal import failed")


PORT_SPEED = [1,10,25,40, 50,100]
UNDEFINED_FEC = 0 ;
FEC_NONE = 1;
FEC_FC = 2;
FEC_RS = 3;

UNDEFINED_AN = 0
AN_OFF = 1
AN_ON = 2


MODEL = 0
WEDGEBF10032X = 1
SESSION_TYPE = ["MODEL", "WEDGEBF10032X"]

ALL_THREADS = []

if os.path.exists(BSP_FILE_PATH):
    SESSION_ID = WEDGEBF10032X
    CPU_PORT = 192
else:
    SESSION_ID = MODEL
    CPU_PORT = 64


op_type = None

WRITE = 1
UPDATE = 2
DELETE = 3

def _Exception():
    exc_type, exc_obj, tb = sys.exc_info()
    f = tb.tb_frame
    lineno = tb.tb_lineno
    filename = f.f_code.co_filename
    linecache.checkcache(filename)
    line = linecache.getline(filename, lineno, f.f_globals)
    return 'EXCEPTION IN ({}, LINE {} "{}"): {}'.format(filename, lineno, line.strip(), exc_obj)

class BfRuntimeGrpcClient:
    def __init__(self, grpc_addr, p4_program_name, client_id, pipe_name):
        self.class_name = type(self).__name__
        self.grpc_addr = grpc_addr # "localhost:50052"
        self.p4_name = p4_program_name
        self.client_id = client_id
        self.pipe_name = pipe_name
        logger.warning("GRPC_ADDRESS: %s" % self.grpc_addr)
        logger.warning("P4_NAME: %s" % self.p4_name)
        logger.warning("CLIENT_ID: %s" % self.client_id)

        try:
            self.interface = gc.ClientInterface(
                self.grpc_addr, client_id=self.client_id, device_id=0
            )
        except Exception as e:
            logger.error("[setUp error]: %s" % e)
            logger.error(
                "RuntimeError: Cannot connect grpc server: Is bf_switch running ?"
            )
            sys.exit(1)

        self.bfrt_info = self.interface.bfrt_info_get(self.p4_name)
        self.port_table = self.bfrt_info.table_get("$PORT")
        self.port_str_info_table = self.bfrt_info.table_get("$PORT_STR_INFO")
        self.stat_table = self.bfrt_info.table_get("$PORT_STAT")
        self.target = gc.Target(device_id=0, pipe_id=0xFFFF)
        self.client_id = int(client_id)
        self.interface.bind_pipeline_config(self.p4_name)

    def tearDown(self):
        self.interface.tear_down_stream()

class BfSubIfCounter(Thread):
    def __init__(self, threadID, name, bfgc, sck_file,pipe_name,subif_counter_interval,):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        self.file = sck_file
        self.die = False
        self.subif_counter_interval= subif_counter_interval
        self.pipe_name = pipe_name

    def run(self):
        try:
            logger.warning("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - loop" % (self.class_name))
                if len(ACTIVE_PORTS.keys())==0:
                    logger.warning("%s - No active ports" % (self.class_name))
                else:
                    logger.debug("%s - ACTIVE_PORTS%s" % (self.class_name, ACTIVE_PORTS.keys()))
                    self.getReadSwitchSubIfCounter()
                sleep(self.subif_counter_interval)

        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - exited with code [%s]" % (self.class_name,_Exception()))
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

        tbl_stats_in.operations_execute(self.bfgc.target, 'Sync')
        tbl_stats_out.operations_execute(self.bfgc.target, 'Sync')
        tbl_stats_pkt_out.operations_execute(self.bfgc.target, 'Sync')

        resp = tbl_vlan_in.entry_get(self.bfgc.target, [], {"from_hw": False})
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            ACTIVE_SUBIFS[data_fields['src']]=[key_fields['ig_md.ingress_id']['value'],
                                  key_fields['hdr.vlan.vid']['value']]

        for port_id in ACTIVE_PORTS.keys():
            SUBIF_COUNTERS.update({port_id:[0,0,0,0]})
        for subif_if in ACTIVE_SUBIFS.keys():
            SUBIF_COUNTERS.update({subif_if:[0,0,0,0]})

        for key in SUBIF_COUNTERS.keys():
            logger.debug("SUBIF_COUNTERS[%s]=%s" % (key,
                                  SUBIF_COUNTERS[key]))

        for counter_id in SUBIF_COUNTERS.keys():
        # read counter from p4 switch via grpc client
            logger.debug("%s - reading counter_id[%s]" % (self.class_name,counter_id))
            # IN counters
            key_list=[tbl_stats_in.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])]
            data_list=None
            stats_in_entry = tbl_stats_in.entry_get(self.bfgc.target,
                                                        key_list,
                                                        {"from-hw": False},
                                                        data_list)

            stats_in = next(stats_in_entry)[0].to_dict()
            logger.debug("INGRESS STATS FOR SUBIF[%s]=%s" % (counter_id,stats_in))

            # OUT counters
            key_list=[tbl_stats_out.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])]
            data_list=None
            stats_out_entry = tbl_stats_out.entry_get(self.bfgc.target,
                                                        key_list,
                                                        {"from-hw": False},
                                                        data_list)
            stats_out = next(stats_out_entry)[0].to_dict()
            logger.debug("EGRESS STATS FOR SUBIF[%s]=%s" % (counter_id,stats_out))

            # PKT_OUT counters
            key_list=[tbl_stats_pkt_out.make_key([gc.KeyTuple("$COUNTER_INDEX", counter_id)])]
            data_list=None
            stats_pkt_out_entry = tbl_stats_pkt_out.entry_get(self.bfgc.target,
                                                        key_list,
                                                        {"from-hw": False},
                                                        data_list)
            stats_pkt_out = next(stats_pkt_out_entry)[0].to_dict()
            logger.debug("EGRESS PKT_OUT STATS FOR SUBIF[%s]=%s" % (counter_id,stats_pkt_out))

            SUBIF_COUNTERS.update({counter_id:[stats_in["$COUNTER_SPEC_PKTS"] - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                               stats_in["$COUNTER_SPEC_BYTES"] - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                                               stats_out["$COUNTER_SPEC_PKTS"] + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                               stats_out["$COUNTER_SPEC_BYTES"] + stats_pkt_out["$COUNTER_SPEC_BYTES"]]})

            logger.debug("tx: ['counter','%s','%s','%s', '%s', '%s'\\n']"
                                % (counter_id,
                                stats_in["$COUNTER_SPEC_PKTS"] - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_in["$COUNTER_SPEC_BYTES"] - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                                stats_out["$COUNTER_SPEC_PKTS"] + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_out["$COUNTER_SPEC_BYTES"] + stats_pkt_out["$COUNTER_SPEC_BYTES"] ))

            self.file.write("counter %s %s %s %s %s\n"
                                % (counter_id,
                                stats_in["$COUNTER_SPEC_PKTS"] - stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_in["$COUNTER_SPEC_BYTES"] - stats_pkt_out["$COUNTER_SPEC_BYTES"],
                                stats_out["$COUNTER_SPEC_PKTS"] + stats_pkt_out["$COUNTER_SPEC_PKTS"],
                                stats_out["$COUNTER_SPEC_BYTES"] + stats_pkt_out["$COUNTER_SPEC_BYTES"]))
            self.file.flush()

    def tearDown(self):
        os._exit(0)
        self.die=True

class BfIfStatus(Thread):
    def __init__(self, threadID, name, bfgc, sck_file,oper_status_interval,):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.bfgc = bfgc
        #self.fil =  sck.makefile("rw")
        self.file = sck_file
        self.die=False
        self.oper_status_interval=oper_status_interval

    def getAllActivePorts(self):
        resp = self.bfgc.port_table.entry_get(self.bfgc.target, [], {"from_hw": False})
        for d, k in resp:
            key_fields = k.to_dict()
            data_fields = d.to_dict()
            return (key_fields['$DEV_PORT']['value'], data_fields['$PORT_NAME'])

    def run(self):
        try:

            logger.warning("%s - main" % (self.class_name))
            while not self.die:
                logger.debug("%s - loop" % (self.class_name))
                logger.debug("%s - %s" % (self.class_name,self.getAllActivePorts()))
                self.getAllActivePorts()
                if len(ACTIVE_PORTS.keys())==0:
                    logger.warning("%s - No active ports" %(self.class_name))
                else:
                    self.getActiveSwitchOperStatus()

                sleep(self.oper_status_interval)


        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning("%s - exited with code [%s]" % (self.class_name,_Exception()))
            self.tearDown()

    def getActiveSwitchOperStatus(self):
        for port_id in ACTIVE_PORTS.keys():
            key_list=[self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])]
            data_list=self.bfgc.port_table.make_data([gc.DataTuple("$PORT_UP")])
            port_entry = self.bfgc.port_table.entry_get(self.bfgc.target,
                                                        key_list,
                                                        {"from-hw": False},
                                                        data_list)
            port = next(port_entry)[0].to_dict()
            #if PORTS_OPER_STATUS.has_key(port_id):
            if port_id in PORTS_OPER_STATUS:
                # notify control plane if needed
                if PORTS_OPER_STATUS[port_id] == port["$PORT_UP"]:
                    logger.debug("%s - PORTS_OPER_STATUS[%s] no state change" % (self.class_name,port_id) )
                    continue
                elif (PORTS_OPER_STATUS[port_id] == True) and (port["$PORT_UP"] == False):
                    self.file.write("state %s %s\n" % (port_id, 0))
                    #self.file.writelines(["state %s %s" % (port_id, 0)])
                    self.file.flush()
                    logger.warning("tx: ['state','%s','0','\\n']" % port_id )
                    PORTS_OPER_STATUS[port_id] = False
                    logger.debug("%s - PORTS_OPER_STATUS[%s] state change to DOWN" % (self.class_name,port_id) )
                elif (PORTS_OPER_STATUS[port_id] == False) and (port["$PORT_UP"] == True):
                    self.file.write("state %s %s\n" % (port_id, 1))
                    #self.file.writelines(["state %s %s" % (port_id, 1)])
                    self.file.flush()
                    logger.warning("tx: ['state','%s','1','\\n']" % port_id )
                    PORTS_OPER_STATUS[port_id] = True
                    logger.debug("%s - PORTS_OPER_STATUS[%s] state change to UP" % (self.class_name,port_id) )

            else:
                logger.warning("%s - PORTS_OPER_STATUS[%s] does not exist, adding it ..." % (self.class_name,port_id) )
                PORTS_OPER_STATUS[port_id]=port["$PORT_UP"]

    def tearDown(self):
        os._exit(0)
        self.die=True

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
                    logger.warning("%s - No active ports" % (self.class_name))
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

class BfForwarder(Thread):
    def __init__(self, threadID, name,platform, bfgc, salgc, sck_file, brdg, mpls, srv6, nat, pbr, tun, poe, mcast, polka, nsh):
        self.class_name = type(self).__name__
        Thread.__init__(self)
        self.threadID = threadID
        self.name = name
        self.platform = platform
        self.bfgc = bfgc
        self.salgc = salgc
        self.brdg = brdg
        self.mpls = mpls
        self.srv6 = srv6
        self.polka = polka
        self.nsh = nsh
        self.nat = nat
        self.pbr = pbr
        self.tun = tun
        self.poe = poe
        self.mcast = mcast
        self.die=False
        self.hairpins = []
        self.mcast_nid = []
        self.mcast_xid = []
        self.file = sck_file
        self.recirc_port = 68;
        self._clearTable()

    def tearDown(self):
        os._exit(0);
        self.bfgc.interface._tear_down_stream()

    def _getTableKeys(self, table_name_key, table_dict, keys):
        try:
            for (d, k) in table_dict[table_name_key].entry_get(self.bfgc.target):
                if k is not None:
                    keys.append(k)
        except Exception as e:
            logger.warning("Error cleaning up: {}".format(e))
        logger.debug("   Keys to delete: %s" % (keys))

    def _delTableKeys(self, table_name_key, table_dict, keys):
        print("  Clearing Table {}".format(table_name_key))
        #table_dict[table_name_key].entry_del(self.bfgc.target, keys)
        for k in keys:
            try:
                logger.debug("   Key to delete: %s" % (k))
                table_dict[table_name_key].entry_del(self.bfgc.target, [k])
            except Exception as e:
                logger.warning("Error cleaning up: {}".format(e))

    def _clearOneTable(self, table_name_key, table_dict):
        keys = []
        self._getTableKeys(table_name_key, table_dict, keys)
        self._delTableKeys(table_name_key, table_dict, keys)


    def _clearTable(self):
        table_dict = {}
        for (
            table_name,
            table_info,
        ) in self.bfgc.bfrt_info.parsed_info.table_info_dict_get().items():
            table_dict[table_info.name_get()] = gc._Table(
                table_info, self.bfgc.interface.reader_writer_interface
            )

        logger.warning("Generic table clearing: (Order not matters)")

        for table_name_key in table_dict.keys():
            if "ig_ctl_pkt_pre_emit" in table_name_key:
                continue
            if "ig_ctl_bundle" in table_name_key:
                continue
            # IN/OUT meters cannot be cleared
            if "policer" in table_name_key:
                continue
            if "hash" in table_name_key:
                continue
            # IN/OUT counters cannot be cleared
            if "stats" in table_name_key:
                continue
            if "stats4" in table_name_key:
                continue
            if "stats6" in table_name_key:
                continue
            if "ig_ctl" in table_name_key:
                self._clearOneTable(table_name_key, table_dict)
            if "eg_ctl" in table_name_key:
                self._clearOneTable(table_name_key, table_dict)

        logger.warning("Bundle specific clearing: (Order matters)")

        nhp_name = "%s.ig_ctl.ig_ctl_bundle.tbl_nexthop_bundle" % self.bfgc.pipe_name
        ase_name = "%s.ig_ctl.ig_ctl_bundle.ase_bundle" % self.bfgc.pipe_name
        apr_name = "%s.ig_ctl.ig_ctl_bundle.apr_bundle" % self.bfgc.pipe_name
        nhp_keys = []
        ase_keys = []
        apr_keys = []
        self._getTableKeys(nhp_name, table_dict, nhp_keys)
        self._getTableKeys(ase_name, table_dict, ase_keys)
        self._getTableKeys(apr_name, table_dict, apr_keys)
        self._delTableKeys(nhp_name, table_dict, nhp_keys)
        self._delTableKeys(ase_name, table_dict, ase_keys)
        self._delTableKeys(apr_name, table_dict, apr_keys)

        logger.warning("Multicast specific clearing: (Order matters)")

        mgid_name = "$pre.mgid"
        node_name = "$pre.node"
        mgid_keys = []
        node_keys = []
        self._getTableKeys(mgid_name, table_dict, mgid_keys)
        self._getTableKeys(node_name, table_dict, node_keys)
        self._delTableKeys(mgid_name, table_dict, mgid_keys)
        self._delTableKeys(node_name, table_dict, node_keys)



    def _processMcastMgidFromControlPlane(
        self,
        op_type,
        mgid,
        nodes,
        exclud,
    ):
        try:
            tbl_name = "$pre.mgid"
            tbl = self.bfgc.bfrt_info.table_get(tbl_name)
            key_field_list = [
                gc.KeyTuple('$MGID', mgid)
            ]
            data_field_list = [
                gc.DataTuple("$MULTICAST_NODE_ID", int_arr_val=nodes),
                gc.DataTuple("$MULTICAST_NODE_L1_XID_VALID", bool_arr_val=exclud),
                gc.DataTuple("$MULTICAST_NODE_L1_XID", int_arr_val=exclud),
            ]
            key_list = [tbl.make_key(key_field_list)]
            data_list = [tbl.make_data(data_field_list)]
        except KeyError as e:
            print("Error preparing entry to control plane: {}".format(e))
            return

        try:
            if op_type == WRITE:
                tbl.entry_add(self.bfgc.target, key_list, data_list)
                #### tbl.entry_mod_inc(self.bfgc.target, key_list, data_list, bfrt_pb2.TableModIncFlag.MOD_INC_ADD)
                logger.debug(
                    "Writing entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            elif op_type == UPDATE:
                tbl.entry_mod(self.bfgc.target, key_list, data_list)
                logger.debug(
                    "Updating entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            elif op_type == DELETE:
                tbl.entry_del(self.bfgc.target, key_list)
                logger.debug(
                    "Deleting entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            else:
                print("Error op_type unknown")

        except gc.BfruntimeRpcException as e:
            print("Error processing entry from control plane: {}".format(e))

        except grpc.RpcError as e:
            print(
                "Grpc channel error "
                "while processing entry from control plane: {}".format(e)
            )


    def _processMcastNodeFromControlPlane(
        self,
        op_type,
        nodeid,
        rid,
        port,
    ):
        try:
            tbl_name = "$pre.node"
            tbl = self.bfgc.bfrt_info.table_get(tbl_name)
            key_field_list = [
                gc.KeyTuple('$MULTICAST_NODE_ID', nodeid)
            ]
            data_field_list = [
                gc.DataTuple("$MULTICAST_RID", rid),
                gc.DataTuple("$DEV_PORT", int_arr_val=[port]),
            ]
            key_list = [tbl.make_key(key_field_list)]
            data_list = [tbl.make_data(data_field_list)]
        except KeyError as e:
            print("Error preparing entry to control plane: {}".format(e))
            return

        try:
            if op_type == WRITE:
                tbl.entry_add(self.bfgc.target, key_list, data_list)
                logger.debug(
                    "Writing entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            elif op_type == UPDATE:
                tbl.entry_mod(self.bfgc.target, key_list, data_list)
                logger.debug(
                    "Updating entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            elif op_type == DELETE:
                tbl.entry_del(self.bfgc.target, key_list)
                logger.debug(
                    "Deleting entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            else:
                print("Error op_type unknown")

        except gc.BfruntimeRpcException as e:
            print("Error processing entry from control plane: {}".format(e))

        except grpc.RpcError as e:
            print(
                "Grpc channel error "
                "while processing entry from control plane: {}".format(e)
            )



    def _processMeterFromControlPlane(
        self,
        op_type,
        tbl_name,
        index,
        bytes,
        interval,
    ):
        if interval == 0:
            kbps = 0
        else:
            kbps = int(bytes * 8 / interval)
        try:
            tbl = self.bfgc.bfrt_info.table_get(tbl_name)
            key_field_list = [
                gc.KeyTuple("$METER_INDEX", index),
            ]
            data_field_list = [
                gc.DataTuple("$METER_SPEC_CIR_KBPS", kbps),
                gc.DataTuple("$METER_SPEC_PIR_KBPS", kbps),
                gc.DataTuple("$METER_SPEC_CBS_KBITS", 1),
                gc.DataTuple("$METER_SPEC_PBS_KBITS", 1),
            ]
            key_list = [tbl.make_key(key_field_list)]
            data_list = [tbl.make_data(data_field_list)]
        except KeyError as e:
            print("Error preparing entry to control plane: {}".format(e))
            return

        try:

            tbl.entry_mod(self.bfgc.target, key_list, data_list)
            logger.debug(
                "Updating entry in table:%s keys:%s act_param:%s",
                tbl_name,
                key_list,
                data_list,
            )

        except gc.BfruntimeRpcException as e:
            print("Error processing entry from control plane: {}".format(e))

        except grpc.RpcError as e:
            print(
                "Grpc channel error "
                "while processing entry from control plane: {}".format(e)
            )

    def _processEntryFromControlPlane(
        self,
        op_type,
        tbl_name,
        key_field_list,
        data_field_list,
        tbl_action_name,
        key_annotation_fields,
        data_annotation_fields,
    ):
        try:
            tbl = self.bfgc.bfrt_info.table_get(tbl_name)

            for annotation in key_annotation_fields.keys():
                tbl.info.key_field_annotation_add(
                    annotation, key_annotation_fields[annotation]
                )

            for annotation in data_annotation_fields.keys():
                tbl.info.data_field_annotation_add(
                    annotation, tbl_action_name, data_annotation_fields[annotation]
                )

            key_list = [tbl.make_key(key_field_list)]
            data_list = [tbl.make_data(data_field_list, tbl_action_name)]
        except KeyError as e:
            print("Error preparing entry to control plane: {}".format(e))
            return

        try:
            if op_type == WRITE:
                tbl.entry_add(self.bfgc.target, key_list, data_list)
                logger.debug(
                    "Writing entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            elif op_type == UPDATE:
                tbl.entry_mod(self.bfgc.target, key_list, data_list)
                logger.debug(
                    "Updating entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            elif op_type == DELETE:
                tbl.entry_del(self.bfgc.target, key_list)
                logger.debug(
                    "Deleting entry in table:%s keys:%s act_param:%s",
                    tbl_name,
                    key_list,
                    data_list,
                )
            else:
                print("Error op_type unknown")

        except gc.BfruntimeRpcException as e:
            print("Error processing entry from control plane: {}".format(e))

        except grpc.RpcError as e:
            print(
                "Grpc channel error "
                "while processing entry from control plane: {}".format(e)
            )

    # rx: ['mtu', '5', '1500', '\n']
    def setPortMTU(self, port_id, mtu_size):
        if port_id in ACTIVE_PORTS.keys():
            key=[self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])]
            data=[self.bfgc.port_table.make_data(
                      [
                          gc.DataTuple("$TX_MTU", mtu_size+18),
                          gc.DataTuple("$RX_MTU", mtu_size+18),
                      ]
                  )]

            self.bfgc.port_table.entry_mod(self.bfgc.target,key,data)
            logger.debug("Port[%s] MTU set to %s" % (port_id,mtu_size))

    def _setPortAdmStatusBF2556X1T(self, port_id, adm_status,
                                   port_speed,fec=0,autoneg=1,flowctrl=0):
        method_name = inspect.currentframe().f_code.co_name
        new_speed = self.salgc.speedTnaToSal(port_speed)
        sal_port = SAL_PORT_ID[port_id]
        if adm_status == 1:
            if self._checkParamCoherence(port_speed,fec,autoneg,flowctrl) == False:
                logger.warning("%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                                 % (self.class_name,port_id,port_speed,fec,autoneg,flowctrl ))
                return None
            try:
                self.salgc.AddPort(sal_port,lane=0,
                                   speed=new_speed,fec=fec,an=autoneg,fc=flowctrl,
                                   enable=adm_status,up=adm_status)

                ACTIVE_PORTS[port_id]=port_speed
            except:
                logger.warning("%s:%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                             % (self.class_name,method_name,port_id,port_speed,fec,autoneg,flowctrl ))
                logger.warning("%s:%s - with code [%s]" % (self.class_name,method_name,_Exception()))
        else:
            try:
                self.salgc.DelPort(port_num=sal_port, lane=0)
                ACTIVE_PORTS.pop(port_id)
            except:
                logger.warning("%s:%s - Error in deleting port [%s]" % (self.class_name,method_name,port_id))

    def _checkParamCoherence(self,port_speed,fec,autoneg,flowctrl):
        if (port_speed == 1) or (port_speed == 10):
            if (fec == FEC_RS):
               return False
        elif (port_speed == 100):
            if (fec == FEC_FC):
               return False
        else:
            return True

    def _getStrFEC(self,fec):
        if fec == FEC_NONE:
            return "BF_FEC_TYP_NONE"
        elif fec == FEC_FC:
            return "BF_FEC_TYP_FC"
        elif fec == FEC_RS:
            return "BF_FEC_TYP_RS"
        else:
            return "BF_FEC_TYP_NONE"

    def _getStrAN(self,autoneg):
        if autoneg == 0:
            return "PM_AN_DEFAULT"
        elif autoneg == AN_OFF:
            return "PM_AN_FORCE_DISABLE"
        elif autoneg == AN_ON:
            return "PM_AN_FORCE_ENABLE"
        else:
            return "PM_AN_DEFAULT"


    def _setPortAdmStatus(self, port_id, adm_status=0,
                          port_speed=10,fec=0,autoneg=1,flowctrl=0):
        # set port_id to adm_status
        method_name = inspect.currentframe().f_code.co_name

        str_port_speed = ""
        str_fec = ""
        str_an = ""

        if adm_status == 1:
            if port_speed in PORT_SPEED:
                str_port_speed = "BF_SPEED_%sG" % (port_speed)
            else:
                logger.error("Unknown port speed: %s", port_speed)
                return

            if self._checkParamCoherence(port_speed,fec,autoneg,flowctrl) == False:
                logger.warning("%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                                 % (self.class_name,port_id,port_speed,fec,autoneg,flowctrl ))
                return None

            str_fec = self._getStrFEC(fec)
            str_an = self._getStrAN(autoneg)

            try:
                self.bfgc.port_table.entry_add(
                    self.bfgc.target,
                    [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])],
                    [
                        self.bfgc.port_table.make_data(
                            [
                                gc.DataTuple("$SPEED", str_val=str_port_speed),
                                gc.DataTuple("$FEC", str_val=str_fec),
                                gc.DataTuple("$AUTO_NEGOTIATION", str_val=str_an),
                                gc.DataTuple("$PORT_ENABLE", bool_val=True),
                            ]
                        )
                    ],
                )
                ACTIVE_PORTS[port_id]=port_speed
                logger.debug("Port[%s] administratively enabled", port_id)
            except:
                logger.warning("%s:%s - Error in enabling port [%s] with parameters:[%s,%s,%s,%s]"
                            % (self.class_name,method_name,port_id,port_speed,fec,autoneg,flowctrl ))
                logger.warning("%s:%s - with code [%s]" % (self.class_name,method_name,_Exception()))
        else:
            try:
                self.bfgc.port_table.entry_del(
                    self.bfgc.target,
                    [self.bfgc.port_table.make_key([gc.KeyTuple("$DEV_PORT", port_id)])]
                )

                ACTIVE_PORTS.pop(port_id)

                logger.debug("Port[%s] administratively disabled", port_id)
            except:
                logger.warning("%s:%s - Error in deleting port [%s]" % (self.class_name,method_name,port_id))


    def setBundleAdmStatus(self, op_type, bundle_id, member_id_list):
        tbl_global_path = "ig_ctl.ig_ctl_bundle"
        tbl_apr_name = "%s.apr_bundle" % (tbl_global_path)
        tbl_ase_name = "%s.ase_bundle" % (tbl_global_path)
        tbl_nexthop_bundle_name = "%s.tbl_nexthop_bundle" % (tbl_global_path)
        max_grp_size = 120
        member_id_list.pop()
        member_id_list = list(map(int, member_id_list))
        member_status_list = []
        # actionprofile p4 objects are actually an object table
        tbl_apr_bundle = self.bfgc.bfrt_info.table_get(tbl_apr_name)
        # actionselector p4 objects are actually an object table
        tbl_ase_bundle = self.bfgc.bfrt_info.table_get(tbl_ase_name)
        # tbl_nexthop_bundle
        tbl_nexthop_bundle = self.bfgc.bfrt_info.table_get(tbl_nexthop_bundle_name)
        member_status_list = []
        for member_id in member_id_list:
            member_status_list.append(True)

        if op_type == WRITE:
            for member_id in member_id_list:
                tbl_apr_bundle_key = [
                    tbl_apr_bundle.make_key(
                        [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                    )
                ]
                tbl_apr_bundle_data = [
                    tbl_apr_bundle.make_data(
                        [gc.DataTuple("port", member_id)],
                        "ig_ctl.ig_ctl_bundle.act_send_to_member",
                    )
                ]

                try:
                   tbl_apr_bundle.entry_add(
                       self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
                   )
                except gc.BfruntimeRpcException as e:
                    logger.warning("bundle_add - %s" %  e.__str__())
                    logger.warning("bundle_add - GRPC error code: %s" % e.grpc_error.code())


            tbl_ase_bundle_key = [
                tbl_ase_bundle.make_key([gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)])
            ]

            tbl_ase_bundle_data = [
                tbl_ase_bundle.make_data(
                    [
                        gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                        gc.DataTuple("$ACTION_MEMBER_ID", int_arr_val=member_id_list),
                        gc.DataTuple(
                            "$ACTION_MEMBER_STATUS", bool_arr_val=member_status_list
                        ),
                    ]
                )
            ]

            tbl_ase_bundle.entry_add(
                self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
            )

            tbl_nexthop_bundle_key = [
                tbl_nexthop_bundle.make_key([gc.KeyTuple("ig_md.output_id", bundle_id)])
            ]

            tbl_nexthop_bundle_data = [
                tbl_nexthop_bundle.make_data(
                    [gc.DataTuple("$SELECTOR_GROUP_ID", bundle_id)]
                )
            ]

            tbl_nexthop_bundle.entry_add(
                self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
            )

        elif op_type == UPDATE:

            bundle_id_entry = tbl_ase_bundle.entry_get(
                self.bfgc.target,
                [
                    tbl_ase_bundle.make_key(
                        [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                    )
                ],
                {"from_hw": False},
            )

            data_dict = next(bundle_id_entry)[0].to_dict()

            mem_dict_recv = {
                data_dict["$ACTION_MEMBER_ID"][i]: data_dict["$ACTION_MEMBER_STATUS"][i]
                for i in range(0, len(data_dict["$ACTION_MEMBER_ID"]))
            }

            member_id_to_add = []
            member_id_to_del = []

            for member_id in member_id_list:
                if not (member_id in mem_dict_recv.keys()):
                    member_id_to_add.append(member_id)

            for member_id in mem_dict_recv.keys():
                if not (member_id in member_id_list):
                    member_id_to_del.append(member_id)

            if len(member_id_to_add) != 0:
                for member_id in member_id_to_add:
                    tbl_apr_bundle_key = [
                        tbl_apr_bundle.make_key(
                            [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                        )
                    ]
                    tbl_apr_bundle_data = [
                        tbl_apr_bundle.make_data(
                            [gc.DataTuple("port", member_id)],
                            "ig_ctl.ig_ctl_bundle.act_send_to_member",
                        )
                    ]

                    tbl_apr_bundle.entry_add(
                        self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
                    )

                tbl_ase_bundle_key = [
                    tbl_ase_bundle.make_key(
                        [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                    )
                ]

                tbl_ase_bundle_data = [
                    tbl_ase_bundle.make_data(
                        [
                            gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                            gc.DataTuple(
                                "$ACTION_MEMBER_ID", int_arr_val=member_id_list
                            ),
                            gc.DataTuple(
                                "$ACTION_MEMBER_STATUS", bool_arr_val=member_status_list
                            ),
                        ]
                    )
                ]

                tbl_ase_bundle.entry_mod(
                    self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
                )

            if len(member_id_to_del) != 0:
                tbl_ase_bundle_key = [
                    tbl_ase_bundle.make_key(
                        [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                    )
                ]

                tbl_ase_bundle_data = [
                    tbl_ase_bundle.make_data(
                        [
                            gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                            gc.DataTuple(
                                "$ACTION_MEMBER_ID", int_arr_val=member_id_list
                            ),
                            gc.DataTuple(
                                "$ACTION_MEMBER_STATUS", bool_arr_val=member_status_list
                            ),
                        ]
                    )
                ]

                tbl_ase_bundle.entry_mod(
                    self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
                )

                for member_id in member_id_to_del:
                    tbl_apr_bundle_key = [
                        tbl_apr_bundle.make_key(
                            [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                        )
                    ]
                    tbl_apr_bundle.entry_del(self.bfgc.target, tbl_apr_bundle_key)

        elif op_type == DELETE:

            bundle_id_entry = tbl_ase_bundle.entry_get(
                self.bfgc.target,
                [
                    tbl_ase_bundle.make_key(
                        [gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)]
                    )
                ],
                {"from_hw": False},
            )

            data_dict = next(bundle_id_entry)[0].to_dict()

            mem_dict_recv = {
                data_dict["$ACTION_MEMBER_ID"][i]: data_dict["$ACTION_MEMBER_STATUS"][i]
                for i in range(0, len(data_dict["$ACTION_MEMBER_ID"]))
            }

            tbl_nexthop_bundle_key = [
                tbl_nexthop_bundle.make_key([gc.KeyTuple("ig_md.output_id", bundle_id)])
            ]

            tbl_nexthop_bundle.entry_del(self.bfgc.target, tbl_nexthop_bundle_key)

            tbl_ase_bundle_key = [
                tbl_ase_bundle.make_key([gc.KeyTuple("$SELECTOR_GROUP_ID", bundle_id)])
            ]
            tbl_ase_bundle.entry_del(self.bfgc.target, tbl_ase_bundle_key)

            for member_id in mem_dict_recv.keys():
                tbl_apr_bundle_key = [
                    tbl_apr_bundle.make_key(
                        [gc.KeyTuple("$ACTION_MEMBER_ID", member_id)]
                    )
                ]
                tbl_apr_bundle.entry_del(self.bfgc.target, tbl_apr_bundle_key)

            if member_id_list.sort() != mem_dict_recv.keys().sort():
                logger.warning(
                    "[setBundleAdmStatus]: "
                    "Member list from Control Plane is different "
                    "from entry list in ActionSelector table"
                )

    def hairpin2recirc(self, port):
        if port in self.hairpins:
            return self.recirc_port
        return port


    def writeHairpinRules(self, op_type, myid, peerid):
        if op_type == WRITE:
            self.hairpins.append(myid)
        elif op_type == DELETE:
            self.hairpins.remove(myid)

        tbl_global_path = "eg_ctl.eg_ctl_hairpin"
        tbl_name = "%s.tbl_hairpin" % (tbl_global_path)
        tbl_action_name = "%s.act_set_recir" % (tbl_global_path)

        key_fields = [gc.KeyTuple("eg_md.output_id", myid)]
        data_fields = [gc.DataTuple("port", peerid)]
        key_annotation_fields = {}
        data_annotation_fields = {}

        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )



        tbl_global_path = "ig_ctl.ig_ctl_bundle"
        tbl_apr_name = "%s.apr_bundle" % (tbl_global_path)
        tbl_ase_name = "%s.ase_bundle" % (tbl_global_path)
        tbl_nexthop_bundle_name = "%s.tbl_nexthop_bundle" % (tbl_global_path)

        max_grp_size = 120
        # actionprofile p4 objects are actually an object table
        tbl_apr_bundle = self.bfgc.bfrt_info.table_get(tbl_apr_name)
        # actionselector p4 objects are actually an object table
        tbl_ase_bundle = self.bfgc.bfrt_info.table_get(tbl_ase_name)
        # tbl_nexthop_bundle
        tbl_nexthop_bundle = self.bfgc.bfrt_info.table_get(tbl_nexthop_bundle_name)

        tbl_apr_bundle_key = [
            tbl_apr_bundle.make_key(
                [gc.KeyTuple("$ACTION_MEMBER_ID", peerid)]
            )
        ]
        tbl_apr_bundle_data = [
            tbl_apr_bundle.make_data(
                [gc.DataTuple("port", peerid)],
                "ig_ctl.ig_ctl_bundle.act_send_to_recir",
            )
        ]
        tbl_ase_bundle_key = [
            tbl_ase_bundle.make_key([gc.KeyTuple("$SELECTOR_GROUP_ID", myid)])
        ]

        tbl_ase_bundle_data = [
            tbl_ase_bundle.make_data(
                [
                    gc.DataTuple("$MAX_GROUP_SIZE", max_grp_size),
                    gc.DataTuple("$ACTION_MEMBER_ID", int_arr_val=[peerid]),
                    gc.DataTuple(
                        "$ACTION_MEMBER_STATUS", bool_arr_val=[1]
                    ),
                ]
            )
        ]


        tbl_nexthop_bundle_key = [
            tbl_nexthop_bundle.make_key([gc.KeyTuple("ig_md.output_id", myid)])
        ]

        tbl_nexthop_bundle_data = [
            tbl_nexthop_bundle.make_data(
                [gc.DataTuple("$SELECTOR_GROUP_ID", myid)]
            )
        ]

        if op_type == WRITE:
            tbl_apr_bundle.entry_add(
                self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
            )
            tbl_ase_bundle.entry_add(
                self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
            )
            tbl_nexthop_bundle.entry_add(
                self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
            )
        elif op_type == UPDATE:
            tbl_apr_bundle.entry_mod(
                self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
            )
            tbl_ase_bundle.entry_mod(
                self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
            )
            tbl_nexthop_bundle.entry_mod(
                self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
            )
        elif op_type == DELETE:
            tbl_apr_bundle.entry_del(
                self.bfgc.target, tbl_apr_bundle_key, tbl_apr_bundle_data
            )
            tbl_ase_bundle.entry_del(
                self.bfgc.target, tbl_ase_bundle_key, tbl_ase_bundle_data
            )
            tbl_nexthop_bundle.entry_del(
                self.bfgc.target, tbl_nexthop_bundle_key, tbl_nexthop_bundle_data
            )



    def writeVrfRules(self, op_type, port, vrf):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path = "ig_ctl.ig_ctl_vrf"
        tbl_name = "%s.tbl_vrf" % (tbl_global_path)
        tbl_action_name = "%s.act_set_vrf" % (tbl_global_path)

        key_fields = [gc.KeyTuple("ig_md.source_id", port)]
        data_fields = [gc.DataTuple("vrf", vrf)]
        key_annotation_fields = {}
        data_annotation_fields = {}

        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeVlanRules(self, op_type, port, main, vlan):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path_1 = "ig_ctl.ig_ctl_vlan_in"
        tbl_name_1 = "%s.tbl_vlan_in" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_iface" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.ingress_id", main),
            gc.KeyTuple("hdr.vlan.vid", vlan),
        ]
        data_field_list_1 = [gc.DataTuple("src", port)]

        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}

        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "eg_ctl.eg_ctl_vlan_out"
        tbl_name_2 = "%s.tbl_vlan_out" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_set_vlan_port" % (tbl_global_path_2)
        key_field_list_2 = [gc.KeyTuple("eg_md.target_id", port)]
        data_field_list_2 = [gc.DataTuple("port", main), gc.DataTuple("vlan", vlan)]
        key_annotation_fields_2 = {}
        data_annotation_fields_2 = {}

        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

        tbl_global_path_3 = "ig_ctl.ig_ctl_outport"
        tbl_name_3 = "%s.tbl_vlan_out" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_set_port_vlan" % (tbl_global_path_3)
        key_field_list_3 = [gc.KeyTuple("ig_md.target_id", port)]
        data_field_list_3 = [gc.DataTuple("port", main)]
        key_annotation_fields_3 = {}
        data_annotation_fields_3 = {}

        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )

    def writeNhop2portRules(self, op_type, nhop, subif, port):
        tbl_global_path = "ig_ctl.ig_ctl_outport"
        tbl_name = "%s.tbl_nexthop" % (tbl_global_path)
        tbl_action_name = "%s.act_set_port_nexthop" % (tbl_global_path)

        key_fields = [gc.KeyTuple("ig_md.nexthop_id", nhop)]
        data_fields = [
            gc.DataTuple("port", port),
            gc.DataTuple("subif", subif),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {}

        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writeBunVlanRules(self, op_type, main, vlan, port):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path_1 = "ig_ctl.ig_ctl_vlan_in"
        tbl_name_1 = "%s.tbl_vlan_in" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_iface" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.ingress_id", main),
            gc.KeyTuple("hdr.vlan.vid", vlan),
        ]
        data_field_list_1 = [gc.DataTuple("src", port)]

        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}

        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

    def writeXconnRules(self, op_type, port, target, lab_tun, lab_loc, lab_rem):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        if self.mpls == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
        tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_mpls_decap_l2vpn" % (tbl_global_path_1)
        key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", lab_loc)]
        data_field_list_1 = [gc.DataTuple("port", port)]
        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
        tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_mpls_decap_l2vpn" % (tbl_global_path_2)
        key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", lab_loc)]
        data_field_list_2 = [gc.DataTuple("port", port)]
        key_annotation_fields_2 = {}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

        tbl_global_path_3 = "ig_ctl.ig_ctl_vrf"
        tbl_name_3 = "%s.tbl_vrf" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_set_mpls_xconn_encap" % (tbl_global_path_3)
        key_field_list_3 = [gc.KeyTuple("ig_md.source_id", port)]
        data_field_list_3 = [
            gc.DataTuple("target", target),
            gc.DataTuple("tunlab", lab_tun),
            gc.DataTuple("svclab", lab_rem),
        ]
        key_annotation_fields_3 = {}
        data_annotation_fields_3 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )

    def writeBrprtRules(self, op_type, port, bridge):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path = "ig_ctl.ig_ctl_vrf"
        tbl_name = "%s.tbl_vrf" % (tbl_global_path)
        tbl_action_name = "%s.act_set_bridge" % (tbl_global_path)

        key_fields = [gc.KeyTuple("ig_md.source_id", port)]
        data_fields = [gc.DataTuple("bridge", bridge)]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeBrlabRules(self, op_type, bridge, label):
        if self.mpls == False:
            return
        if self.brdg == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
        tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_mpls_decap_vpls" % (tbl_global_path_1)
        key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", label)]
        data_field_list_1 = [gc.DataTuple("bridge", bridge)]
        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
        tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_mpls_decap_vpls" % (tbl_global_path_2)
        key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", label)]
        data_field_list_2 = [gc.DataTuple("bridge", bridge)]
        key_annotation_fields_2 = {}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

    def writeBrvplsRules(self, op_type, bridge, addr, port, labtun, labsvc):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        if self.brdg == False:
            return
        if self.mpls == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
        tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        ]
        data_field_list_1 = []
        key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
        tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
        sap_type = 4
        tbl_action_name_2 = "%s.act_set_bridge_vpls" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            # gc.KeyTuple('ig_md.sap_type', sap_type),
            gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        ]
        data_field_list_2 = [
            gc.DataTuple("port", port),
            gc.DataTuple("lab_tun", labtun),
            gc.DataTuple("lab_svc", labsvc),
        ]
        key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

    def writeBrmacRules(self, op_type, bridge, addr, port):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        if self.brdg == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
        tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        ]
        data_field_list_1 = []
        key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
        tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_set_bridge_out" % (tbl_global_path_2)
        sap_type = 4
        key_field_list_2 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            # gc.KeyTuple('ig_md.sap_type', sap_type),
            gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        ]
        data_field_list_2 = [gc.DataTuple("port", port)]
        key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

    def writeRoumacRules(self, op_type, bridge, addr, nexthop, ppp):
        # for any reason, control plane is sending a msg
        # with port=-1

        if self.brdg == False:
            return
        if ppp != 0:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
        tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        ]
        data_field_list_1 = []
        key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
        tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_set_bridge_routed" % (tbl_global_path_2)
        sap_type = 4
        key_field_list_2 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            # gc.KeyTuple('ig_md.sap_type', sap_type),
            gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        ]
        data_field_list_2 = [gc.DataTuple("nexthop", nexthop)]
        key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )


    def writePolkaPolyRules(self, op_type, poly):
        if self.polka == False:
            return
        try:
            tbl = self.bfgc.bfrt_info.table_get("ig_ctl.ig_ctl_polka.hash.algorithm")
            data_field_list = [
                gc.DataTuple("polynomial", (poly & 0xffff)),
            ]
            data_list = tbl.make_data(data_field_list, "user_defined")
            tbl.default_entry_set(self.bfgc.target, data_list)
        except gc.BfruntimeRpcException as e:
            print("Error processing entry from control plane: {}".format(e))

        except grpc.RpcError as e:
            print(
                "Grpc channel error "
                "while processing entry from control plane: {}".format(e)
            )


    def writePolkaIndexRules(self, op_type, idx, vrf, hop):
        if self.polka == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_polka"
        tbl_name = "%s.tbl_polka" % (tbl_global_path)
        tbl_action_name = "%s.act_forward" % (tbl_global_path)
        key_fields = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("ig_md.polka_next", idx),
        ]
        data_fields = [
            gc.DataTuple("nexthop_id", hop),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writePolkaOwnRules(self, op_type, idx, vrf):
        if self.polka == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_polka"
        tbl_name = "%s.tbl_polka" % (tbl_global_path)
        tbl_action_name = "%s.act_route" % (tbl_global_path)
        key_fields = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("ig_md.polka_next", idx),
        ]
        data_fields = [
        ]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writeForwardRules4(self, op_type, dst_ip_addr, dst_net_mask, port, vrf):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path = "ig_ctl.ig_ctl_ipv4"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv4b"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4b.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv4b.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeForwardRules6(self, op_type, dst_ip_addr, dst_net_mask, port, vrf):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6b"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6b.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv6b.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeVpnRules4(
        self, op_type, dst_ip_addr, dst_net_mask, port, vrf, egress_label, vpn_label
    ):
        if self.mpls == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv4"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_mpls2_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [
            gc.DataTuple("vpn_label", vpn_label),
            gc.DataTuple("egress_label", egress_label),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeVpnRules6(
        self, op_type, dst_ip_addr, dst_net_mask, port, vrf, egress_label, vpn_label
    ):
        if self.mpls == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_mpls2_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [
            gc.DataTuple("vpn_label", vpn_label),
            gc.DataTuple("egress_label", egress_label),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeGlobRules4(
        self, op_type, dst_ip_addr, dst_net_mask, port, vrf, egress_label
    ):
        if self.mpls == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv4"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_mpls1_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [
            gc.DataTuple("egress_label", egress_label),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeGlobRules6(
        self, op_type, dst_ip_addr, dst_net_mask, port, vrf, egress_label
    ):
        if self.mpls == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_mpls1_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [
            gc.DataTuple("egress_label", egress_label),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeSrvRules4(self, op_type, dst_ip_addr, dst_net_mask, port, vrf, target):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv4"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_srv_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("target", target), gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
        data_annotation_fields = {"target": "ipv6"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeSrvRules6(self, op_type, dst_ip_addr, dst_net_mask, port, vrf, target):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_srv_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("target", target), gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {"target": "ipv6"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writePolkaRules4(
        self, op_type, dst_ip_addr, dst_net_mask, port, vrf, routeid
    ):
        if self.polka == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv4"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_polka_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [
            gc.DataTuple("routeid", bytearray.fromhex(routeid)),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
        data_annotation_fields = {"routeid":"bytes"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writePolkaRules6(
        self, op_type, dst_ip_addr, dst_net_mask, port, vrf, routeid
    ):
        if self.polka == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_polka_encap_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [
            gc.DataTuple("routeid", bytearray.fromhex(routeid)),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {"routeid":"bytes"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeMyaddrRules4(self, op_type, dst_ip_addr, dst_net_mask, vrf):
        tbl_global_path = "ig_ctl.ig_ctl_ipv4"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_cpl_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = []
        key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv4b"
        tbl_name = "%s.tbl_ipv4_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_cpl_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4b.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = []
        key_annotation_fields = {"hdr.ipv4b.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeMyaddrRules6(self, op_type, dst_ip_addr, dst_net_mask, vrf):
        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_cpl_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = []
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {}
        annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6b"
        tbl_name = "%s.tbl_ipv6_fib_lpm" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_cpl_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6b.dst_addr", dst_ip_addr, prefix_len=dst_net_mask),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = []
        key_annotation_fields = {"hdr.ipv6b.dst_addr": "ipv6"}
        data_annotation_fields = {}
        annotation_fields = {"hdr.ipv6b.dst_addr": "ipv6"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeNexthopRules(self, op_type, nexthop, dst_mac_addr, src_mac_addr, port):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path = "eg_ctl.eg_ctl_nexthop"
        tbl_name = "%s.tbl_nexthop" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_fib_hit" % (tbl_global_path)

        key_fields = [gc.KeyTuple("eg_md.nexthop_id", nexthop)]
        data_fields = [
            gc.DataTuple("dst_mac_addr", dst_mac_addr),
            gc.DataTuple("src_mac_addr", src_mac_addr),
            gc.DataTuple("egress_port", port),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {"dst_mac_addr": "mac", "src_mac_addr": "mac"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeNeighborRules4(self, op_type, dst_ip_addr, port, vrf):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path = "ig_ctl.ig_ctl_ipv4"
        tbl_name = "%s.tbl_ipv4_fib_host" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4.dst_addr", dst_ip_addr),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv4.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv4b"
        tbl_name = "%s.tbl_ipv4_fib_host" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv4_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv4b.dst_addr", dst_ip_addr),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv4b.dst_addr": "ipv4"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeNeighborRules6(self, op_type, dst_ip_addr, port, vrf):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_host" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6.dst_addr", dst_ip_addr),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6b"
        tbl_name = "%s.tbl_ipv6_fib_host" % (tbl_global_path)
        tbl_action_name = "%s.act_ipv6_set_nexthop" % (tbl_global_path)

        key_fields = [
            gc.KeyTuple("hdr.ipv6b.dst_addr", dst_ip_addr),
            gc.KeyTuple("ig_md.vrf", vrf),
        ]
        data_fields = [gc.DataTuple("nexthop_id", port)]
        key_annotation_fields = {"hdr.ipv6b.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_fields,
            data_fields,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeMplsRules(self, op_type, dst_label, new_label, port):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        if self.mpls == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
        tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_mpls_swap0_set_nexthop" % (tbl_global_path_1)
        key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", (dst_label))]
        data_field_list_1 = [
            gc.DataTuple("egress_label", new_label),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
        tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_mpls_swap1_set_nexthop" % (tbl_global_path_2)
        key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", (dst_label))]
        data_field_list_2 = [
            gc.DataTuple("egress_label", new_label),
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields_2 = {}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

    def writeUnMplsRules(self, op_type, dst_label, port):
        # for any reason, control plane is sending a msg
        # with port=-1
        if port < 0:
            return

        if self.mpls == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
        tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_mpls_decap_set_nexthop" % (tbl_global_path_1)
        key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", (dst_label))]
        data_field_list_1 = [
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
        tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_mpls_decap_set_nexthop" % (tbl_global_path_2)
        key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", (dst_label))]
        data_field_list_2 = [
            gc.DataTuple("nexthop_id", port),
        ]
        key_annotation_fields_2 = {}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

    def writeCpuMplsRules(self, op_type, dst_label):
        if self.mpls == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
        tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_mpls_cpulabel" % (tbl_global_path_1)
        key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", (dst_label))]
        data_field_list_1 = []
        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
        tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_mpls_cpulabel" % (tbl_global_path_2)
        key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", (dst_label))]
        data_field_list_2 = []
        key_annotation_fields_2 = {}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )


    def writeMyMplsRules(self, op_type, dst_label, vrf):
        if self.mpls == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_mpls"
        tbl_name_1 = "%s.tbl_mpls_fib" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_mpls_decap_ipv4" % (tbl_global_path_1)
        key_field_list_1 = [gc.KeyTuple("hdr.mpls0.label", (dst_label))]
        data_field_list_1 = [gc.DataTuple("vrf", vrf)]
        key_annotation_fields_1 = {}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_mpls"
        tbl_name_2 = "%s.tbl_mpls_fib_decap" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_mpls_decap_l3vpn" % (tbl_global_path_2)
        key_field_list_2 = [gc.KeyTuple("hdr.mpls1.label", (dst_label))]
        data_field_list_2 = [gc.DataTuple("vrf", vrf)]
        key_annotation_fields_2 = {}
        data_annotation_fields_2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )


    def writeNshFwdRules(self, op_type, sp, si, prt, src, dst, tsp, tsi):
        if self.nsh == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_nsh"
        tbl_name = "%s.tbl_nsh" % (tbl_global_path)
        tbl_action_name = "%s.act_forward" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("hdr.nsh.sp", (sp)),
            gc.KeyTuple("hdr.nsh.si", (si)),
        ]
        data_field_list = [
            gc.DataTuple("port", prt),
            gc.DataTuple("src", src),
            gc.DataTuple("dst", dst),
            gc.DataTuple("sp", tsp),
            gc.DataTuple("si", tsi),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {"dst": "mac", "src": "mac"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeNshLocRules(self, op_type, sp, si, vrf):
        if self.nsh == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_nsh"
        tbl_name = "%s.tbl_nsh" % (tbl_global_path)
        tbl_action_name = "%s.act_route" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("hdr.nsh.sp", (sp)),
            gc.KeyTuple("hdr.nsh.si", (si)),
        ]
        data_field_list = [gc.DataTuple("vrf", vrf)]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writeMySrv4rules(self, op_type, glob, dst_addr, vrf):
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_host" % (tbl_global_path)
        tbl_action_name = "%s.act_srv_decap_ipv4" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", (glob)),
            gc.KeyTuple("hdr.ipv6.dst_addr", (dst_addr)),
        ]
        data_field_list = [gc.DataTuple("vrf", vrf)]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeMySrv6rules(self, op_type, glob, dst_addr, vrf):
        if self.srv6 == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_ipv6"
        tbl_name = "%s.tbl_ipv6_fib_host" % (tbl_global_path)
        tbl_action_name = "%s.act_srv_decap_ipv6" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", (glob)),
            gc.KeyTuple("hdr.ipv6.dst_addr", (dst_addr)),
        ]
        data_field_list = [gc.DataTuple("vrf", vrf)]
        key_annotation_fields = {"hdr.ipv6.dst_addr": "ipv6"}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeCopp4Rules(
        self, op_type, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_copp"
        tbl_name = "%s.tbl_ipv4_copp" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeCopp6Rules(
        self, op_type, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_copp"
        tbl_name = "%s.tbl_ipv6_copp" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )





    def writeNatCfgRules4(
        self, op_type, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.nat == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_nat"
        tbl_name = "%s.tbl_ipv4_nat_cfg" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writeNatCfgRules6(
        self, op_type, vrf, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.nat == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_nat"
        tbl_name = "%s.tbl_ipv6_nat_cfg" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writeNatTrnsRules4(
        self, op_type, vrf, proto, osa, osp, ota, otp, nsa, nsp, nta, ntp
    ):
        if self.nat == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_nat"
        tbl_name = "%s.tbl_ipv4_nat_trns" % (tbl_global_path)
        tbl_action_name = "%s.act_rewrite_ipv4prt%s" % (tbl_global_path, str(proto))
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.protocol", proto),
            gc.KeyTuple("hdr.ipv4.src_addr", osa),
            gc.KeyTuple("hdr.ipv4.dst_addr", ota),
            gc.KeyTuple("ig_md.layer4_srcprt", osp),
            gc.KeyTuple("ig_md.layer4_dstprt", otp),
        ]
        data_field_list = [
            gc.DataTuple("srcadr", nsa),
            gc.DataTuple("trgadr", nta),
            gc.DataTuple("srcprt", nsp),
            gc.DataTuple("trgprt", ntp),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {
            "srcadr": "ipv4",
            "trgadr": "ipv4",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writeNatTrnsRules6(
        self, op_type, vrf, proto, osa, osp, ota, otp, nsa, nsp, nta, ntp
    ):
        if self.nat == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_nat"
        tbl_name = "%s.tbl_ipv6_nat_trns" % (tbl_global_path)
        tbl_action_name = "%s.act_rewrite_ipv6prt%s" % (tbl_global_path, str(proto))
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.next_hdr", proto),
            gc.KeyTuple("hdr.ipv6.src_addr", osa),
            gc.KeyTuple("hdr.ipv6.dst_addr", ota),
            gc.KeyTuple("ig_md.layer4_srcprt", osp),
            gc.KeyTuple("ig_md.layer4_dstprt", otp),
        ]
        data_field_list = [
            gc.DataTuple("srcadr", nsa),
            gc.DataTuple("trgadr", nta),
            gc.DataTuple("srcprt", nsp),
            gc.DataTuple("trgprt", ntp),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {
            "srcadr": "ipv6",
            "trgadr": "ipv6",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )



    def writePbrNormRules4(
        self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv4_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_normal" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writePbrNormRules6(
        self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv6_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_normal" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )





    def writePbrVrfRules4(
        self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv4_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_setvrf" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = [
            gc.DataTuple("vrf_id", tvrf),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writePbrVrfRules6(
        self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv6_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_setvrf" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = [
            gc.DataTuple("vrf_id", tvrf),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )





    def writePbrHopRules4(
        self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv4_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_sethop" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = [
            gc.DataTuple("vrf_id", tvrf),
            gc.DataTuple("nexthop_id", thop),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writePbrHopRules6(
        self, op_type, vrf, tvrf, thop, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv6_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_sethop" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = [
            gc.DataTuple("vrf_id", tvrf),
            gc.DataTuple("nexthop_id", thop),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )





    def writePbrLabRules4(
        self, op_type, vrf, tvrf, thop, tlab, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv4_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_setlabel4" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = [
            gc.DataTuple("vrf_id", tvrf),
            gc.DataTuple("nexthop_id", thop),
            gc.DataTuple("label_val", tlab),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writePbrLabRules6(
        self, op_type, vrf, tvrf, thop, tlab, pri, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        if self.pbr == False:
            return
        tbl_global_path = "ig_ctl.ig_ctl_pbr"
        tbl_name = "%s.tbl_ipv6_pbr" % (tbl_global_path)
        tbl_action_name = "%s.act_setlabel6" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = [
            gc.DataTuple("vrf_id", tvrf),
            gc.DataTuple("nexthop_id", thop),
            gc.DataTuple("label_val", tlab),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )




    def writeGre4rules(
        self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac
    ):
        if self.tun == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_1 = "%s.tbl_tunnel4" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_tunnel_gre" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.protocol", 47),
            gc.KeyTuple("hdr.ipv4.src_addr", dip),
            gc.KeyTuple("hdr.ipv4.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 0),
            gc.KeyTuple("ig_md.layer4_dstprt", 0),
        ]
        data_field_list_1 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_1 = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields_1 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )
        tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
        tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_ipv4_gre4" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("eg_md.nexthop_id", nexthop),
        ]
        data_field_list_2 = [
            gc.DataTuple("dst_mac_addr", dmac),
            gc.DataTuple("src_mac_addr", smac),
            gc.DataTuple("egress_port", phport),
            gc.DataTuple("acl_port", port),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_ip_addr", dip),
        ]
        key_annotation_fields_2 = {
        }
        data_annotation_fields_2 = {
            "src_mac_addr": "mac",
            "dst_mac_addr": "mac",
            "src_ip_addr": "ipv4",
            "dst_ip_addr": "ipv4",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )




    def writeGre6rules(
        self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac
    ):
        if self.tun == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_1 = "%s.tbl_tunnel6" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_tunnel_gre" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.next_hdr", 47),
            gc.KeyTuple("hdr.ipv6.src_addr", dip),
            gc.KeyTuple("hdr.ipv6.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 0),
            gc.KeyTuple("ig_md.layer4_dstprt", 0),
        ]
        data_field_list_1 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_1 = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields_1 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )
        tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
        tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_ipv4_gre6" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("eg_md.nexthop_id", nexthop),
        ]
        data_field_list_2 = [
            gc.DataTuple("dst_mac_addr", dmac),
            gc.DataTuple("src_mac_addr", smac),
            gc.DataTuple("egress_port", phport),
            gc.DataTuple("acl_port", port),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_ip_addr", dip),
        ]
        key_annotation_fields_2 = {
        }
        data_annotation_fields_2 = {
            "src_mac_addr": "mac",
            "dst_mac_addr": "mac",
            "src_ip_addr": "ipv6",
            "dst_ip_addr": "ipv6",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )



    def writeIpip4rules(
        self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac
    ):
        if self.tun == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_1 = "%s.tbl_tunnel4" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_tunnel_ip4ip" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.protocol", 4),
            gc.KeyTuple("hdr.ipv4.src_addr", dip),
            gc.KeyTuple("hdr.ipv4.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 0),
            gc.KeyTuple("ig_md.layer4_dstprt", 0),
        ]
        data_field_list_1 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_1 = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields_1 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )
        tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_3 = "%s.tbl_tunnel4" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_tunnel_ip6ip" % (tbl_global_path_3)
        key_field_list_3 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.protocol", 41),
            gc.KeyTuple("hdr.ipv4.src_addr", dip),
            gc.KeyTuple("hdr.ipv4.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 0),
            gc.KeyTuple("ig_md.layer4_dstprt", 0),
        ]
        data_field_list_3 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_3 = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields_3 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )
        tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
        tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_ipv4_ipip4" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("eg_md.nexthop_id", nexthop),
        ]
        data_field_list_2 = [
            gc.DataTuple("dst_mac_addr", dmac),
            gc.DataTuple("src_mac_addr", smac),
            gc.DataTuple("egress_port", phport),
            gc.DataTuple("acl_port", port),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_ip_addr", dip),
        ]
        key_annotation_fields_2 = {
        }
        data_annotation_fields_2 = {
            "src_mac_addr": "mac",
            "dst_mac_addr": "mac",
            "src_ip_addr": "ipv4",
            "dst_ip_addr": "ipv4",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )


    def writeIpip6rules(
        self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac
    ):
        if self.tun == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_1 = "%s.tbl_tunnel6" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_tunnel_ip4ip" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.next_hdr", 4),
            gc.KeyTuple("hdr.ipv6.src_addr", dip),
            gc.KeyTuple("hdr.ipv6.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 0),
            gc.KeyTuple("ig_md.layer4_dstprt", 0),
        ]
        data_field_list_1 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_1 = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields_1 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )
        tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_3 = "%s.tbl_tunnel6" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_tunnel_ip6ip" % (tbl_global_path_3)
        key_field_list_3 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.next_hdr", 41),
            gc.KeyTuple("hdr.ipv6.src_addr", dip),
            gc.KeyTuple("hdr.ipv6.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 0),
            gc.KeyTuple("ig_md.layer4_dstprt", 0),
        ]
        data_field_list_3 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_3 = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields_3 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )
        tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
        tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_ipv4_ipip6" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("eg_md.nexthop_id", nexthop),
        ]
        data_field_list_2 = [
            gc.DataTuple("dst_mac_addr", dmac),
            gc.DataTuple("src_mac_addr", smac),
            gc.DataTuple("egress_port", phport),
            gc.DataTuple("acl_port", port),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_ip_addr", dip),
        ]
        key_annotation_fields_2 = {
        }
        data_annotation_fields_2 = {
            "src_mac_addr": "mac",
            "dst_mac_addr": "mac",
            "src_ip_addr": "ipv6",
            "dst_ip_addr": "ipv6",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )




    def writePppoeRules(
        self, op_type, port, phport, nexthop, vrf, ses, dmac, smac
    ):
        if self.poe == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_pppoe"
        tbl_name_1 = "%s.tbl_pppoe" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_pppoe_data" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.source_id", phport),
            gc.KeyTuple("hdr.pppoeD.session", ses),
        ]
        data_field_list_1 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_1 = {
        }
        data_annotation_fields_1 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )
        tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
        tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_ipv4_pppoe" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("eg_md.nexthop_id", nexthop),
        ]
        data_field_list_2 = [
            gc.DataTuple("dst_mac_addr", dmac),
            gc.DataTuple("src_mac_addr", smac),
            gc.DataTuple("egress_port", phport),
            gc.DataTuple("acl_port", port),
            gc.DataTuple("session", ses),
        ]
        key_annotation_fields_2 = {
        }
        data_annotation_fields_2 = {
            "src_mac_addr": "mac",
            "dst_mac_addr": "mac",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )



    def writeL2tp4rules(
        self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt, tid
    ):
        if self.tun == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_1 = "%s.tbl_tunnel4" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_tunnel_l2tp" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.protocol", 17),
            gc.KeyTuple("hdr.ipv4.src_addr", dip),
            gc.KeyTuple("hdr.ipv4.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", dprt),
            gc.KeyTuple("ig_md.layer4_dstprt", sprt),
        ]
        data_field_list_1 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_1 = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields_1 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )
        tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
        tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_ipv4_l2tp4" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("eg_md.nexthop_id", nexthop),
        ]
        data_field_list_2 = [
            gc.DataTuple("dst_mac_addr", dmac),
            gc.DataTuple("src_mac_addr", smac),
            gc.DataTuple("egress_port", phport),
            gc.DataTuple("acl_port", port),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_ip_addr", dip),
            gc.DataTuple("src_port", sprt),
            gc.DataTuple("dst_port", dprt),
            gc.DataTuple("tunnel_id", tid),
        ]
        key_annotation_fields_2 = {
        }
        data_annotation_fields_2 = {
            "src_mac_addr": "mac",
            "dst_mac_addr": "mac",
            "src_ip_addr": "ipv4",
            "dst_ip_addr": "ipv4",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )



    def writeL2tp6rules(
        self, op_type, nexthop, port, phport, sip, dip, dmac, vrf, smac, sprt, dprt, tid
    ):
        if self.tun == False:
            return
        tbl_global_path_1 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_1 = "%s.tbl_tunnel6" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_tunnel_l2tp" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.next_hdr", 17),
            gc.KeyTuple("hdr.ipv6.src_addr", dip),
            gc.KeyTuple("hdr.ipv6.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", dprt),
            gc.KeyTuple("ig_md.layer4_dstprt", sprt),
        ]
        data_field_list_1 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_1 = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields_1 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )
        tbl_global_path_2 = "eg_ctl.eg_ctl_nexthop"
        tbl_name_2 = "%s.tbl_nexthop" % (tbl_global_path_2)
        tbl_action_name_2 = "%s.act_ipv4_l2tp6" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("eg_md.nexthop_id", nexthop),
        ]
        data_field_list_2 = [
            gc.DataTuple("dst_mac_addr", dmac),
            gc.DataTuple("src_mac_addr", smac),
            gc.DataTuple("egress_port", phport),
            gc.DataTuple("acl_port", port),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_ip_addr", dip),
            gc.DataTuple("src_port", sprt),
            gc.DataTuple("dst_port", dprt),
            gc.DataTuple("tunnel_id", tid),
        ]
        key_annotation_fields_2 = {
        }
        data_annotation_fields_2 = {
            "src_mac_addr": "mac",
            "dst_mac_addr": "mac",
            "src_ip_addr": "ipv6",
            "dst_ip_addr": "ipv6",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )



    def writeVxlan4rules(
        self, op_type, bridge, addr, sip, dip, nexthop, instance, vrf, port
    ):
        if self.tun == False:
            return
        if self.brdg == False:
            return

        tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
        tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        ]
        data_field_list_1 = []
        key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
        tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
        sap_type = 4
        tbl_action_name_2 = "%s.act_set_bridge_vxlan4" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            # gc.KeyTuple('ig_md.sap_type', sap_type),
            gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        ]
        data_field_list_2 = [
            gc.DataTuple("nexthop", nexthop),
            gc.DataTuple("dst_ip_addr", dip),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("instance", instance),
        ]
        key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
        data_annotation_fields_2 = {
            "dst_ip_addr": "ipv4",
            "src_ip_addr": "ipv4",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

        tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_3 = "%s.tbl_tunnel4" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_tunnel_vxlan" % (tbl_global_path_3)
        key_field_list_3 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.protocol", 17),
            gc.KeyTuple("hdr.ipv4.src_addr", dip),
            gc.KeyTuple("hdr.ipv4.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 4789),
            gc.KeyTuple("ig_md.layer4_dstprt", 4789),
        ]
        data_field_list_3 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_3 = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields_3 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )



    def writeVxlan6rules(
        self, op_type, bridge, addr, sip, dip, nexthop, instance, vrf, port
    ):
        if self.tun == False:
            return
        if self.brdg == False:
            return

        tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
        tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        ]
        data_field_list_1 = []
        key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
        tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
        sap_type = 4
        tbl_action_name_2 = "%s.act_set_bridge_vxlan6" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            # gc.KeyTuple('ig_md.sap_type', sap_type),
            gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        ]
        data_field_list_2 = [
            gc.DataTuple("nexthop", nexthop),
            gc.DataTuple("dst_ip_addr", dip),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("instance", instance),
        ]
        key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
        data_annotation_fields_2 = {
            "dst_ip_addr": "ipv6",
            "src_ip_addr": "ipv6",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

        tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_3 = "%s.tbl_tunnel6" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_tunnel_vxlan" % (tbl_global_path_3)
        key_field_list_3 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.next_hdr", 17),
            gc.KeyTuple("hdr.ipv6.src_addr", dip),
            gc.KeyTuple("hdr.ipv6.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", 4789),
            gc.KeyTuple("ig_md.layer4_dstprt", 4789),
        ]
        data_field_list_3 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_3 = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields_3 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )




    def writePckoudp4rules(
        self, op_type, bridge, addr, sip, dip, sprt, dprt, nexthop, vrf, port
    ):
        if self.tun == False:
            return
        if self.brdg == False:
            return

        tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
        tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        ]
        data_field_list_1 = []
        key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
        tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
        sap_type = 4
        tbl_action_name_2 = "%s.act_set_bridge_pckoudp4" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            # gc.KeyTuple('ig_md.sap_type', sap_type),
            gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        ]
        data_field_list_2 = [
            gc.DataTuple("nexthop", nexthop),
            gc.DataTuple("dst_ip_addr", dip),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_port", dprt),
            gc.DataTuple("src_port", sprt),
        ]
        key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
        data_annotation_fields_2 = {
            "dst_ip_addr": "ipv4",
            "src_ip_addr": "ipv4",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

        tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_3 = "%s.tbl_tunnel4" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_tunnel_pckoudp" % (tbl_global_path_3)
        key_field_list_3 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.protocol", 17),
            gc.KeyTuple("hdr.ipv4.src_addr", dip),
            gc.KeyTuple("hdr.ipv4.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", dprt),
            gc.KeyTuple("ig_md.layer4_dstprt", sprt),
        ]
        data_field_list_3 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_3 = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields_3 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )




    def writePckoudp6rules(
        self, op_type, bridge, addr, sip, dip, sprt, dprt, nexthop, vrf, port
    ):
        if self.tun == False:
            return
        if self.brdg == False:
            return

        tbl_global_path_1 = "ig_ctl.ig_ctl_bridge"
        tbl_name_1 = "%s.tbl_bridge_learn" % (tbl_global_path_1)
        tbl_action_name_1 = "%s.act_set_bridge_port" % (tbl_global_path_1)
        key_field_list_1 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            gc.KeyTuple("hdr.ethernet.src_mac_addr", addr),
        ]
        data_field_list_1 = []
        key_annotation_fields_1 = {"hdr.ethernet.src_mac_addr": "mac"}
        data_annotation_fields_1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_1,
            key_field_list_1,
            data_field_list_1,
            tbl_action_name_1,
            key_annotation_fields_1,
            data_annotation_fields_1,
        )

        tbl_global_path_2 = "ig_ctl.ig_ctl_bridge"
        tbl_name_2 = "%s.tbl_bridge_target" % (tbl_global_path_2)
        sap_type = 4
        tbl_action_name_2 = "%s.act_set_bridge_pckoudp6" % (tbl_global_path_2)
        key_field_list_2 = [
            gc.KeyTuple("ig_md.bridge_id", bridge),
            # gc.KeyTuple('ig_md.sap_type', sap_type),
            gc.KeyTuple("hdr.ethernet.dst_mac_addr", addr),
        ]
        data_field_list_2 = [
            gc.DataTuple("nexthop", nexthop),
            gc.DataTuple("dst_ip_addr", dip),
            gc.DataTuple("src_ip_addr", sip),
            gc.DataTuple("dst_port", dprt),
            gc.DataTuple("src_port", sprt),
        ]
        key_annotation_fields_2 = {"hdr.ethernet.dst_mac_addr": "mac"}
        data_annotation_fields_2 = {
            "dst_ip_addr": "ipv6",
            "src_ip_addr": "ipv6",
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_2,
            key_field_list_2,
            data_field_list_2,
            tbl_action_name_2,
            key_annotation_fields_2,
            data_annotation_fields_2,
        )

        tbl_global_path_3 = "ig_ctl.ig_ctl_tunnel"
        tbl_name_3 = "%s.tbl_tunnel6" % (tbl_global_path_3)
        tbl_action_name_3 = "%s.act_tunnel_pckoudp" % (tbl_global_path_3)
        key_field_list_3 = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.next_hdr", 17),
            gc.KeyTuple("hdr.ipv6.src_addr", dip),
            gc.KeyTuple("hdr.ipv6.dst_addr", sip),
            gc.KeyTuple("ig_md.layer4_srcprt", dprt),
            gc.KeyTuple("ig_md.layer4_dstprt", sprt),
        ]
        data_field_list_3 = [
            gc.DataTuple("port", port),
        ]
        key_annotation_fields_3 = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields_3 = {
        }
        self._processEntryFromControlPlane(
            op_type,
            tbl_name_3,
            key_field_list_3,
            data_field_list_3,
            tbl_action_name_3,
            key_annotation_fields_3,
            data_annotation_fields_3,
        )



    def writeInAcl4Rules(
        self, op_type, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_acl_in"
        tbl_name = "%s.tbl_ipv4_acl" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.source_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeOutAcl4Rules(
        self, op_type, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_acl_out"
        tbl_name = "%s.tbl_ipv4_acl" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.aclport_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeInAcl6Rules(
        self, op_type, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_acl_in"
        tbl_name = "%s.tbl_ipv6_acl" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.source_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeOutAcl6Rules(
        self, op_type, port, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_acl_out"
        tbl_name = "%s.tbl_ipv6_acl" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.aclport_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = []
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )




    def writeInQos4Rules(
        self, op_type, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_qos_in"
        tbl_name = "%s.tbl_ipv4_qos" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.source_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = [
             gc.DataTuple("metid", meter),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeOutQos4Rules(
        self, op_type, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_qos_out"
        tbl_name = "%s.tbl_ipv4_qos" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.aclport_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = [
             gc.DataTuple("metid", meter),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeInQos6Rules(
        self, op_type, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_qos_in"
        tbl_name = "%s.tbl_ipv6_qos" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.source_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = [
             gc.DataTuple("metid", meter),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )

    def writeOutQos6Rules(
        self, op_type, port, meter, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_qos_out"
        tbl_name = "%s.tbl_ipv6_qos" % (tbl_global_path)
        tbl_action_name = "%s.act_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.aclport_id", port),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = [
             gc.DataTuple("metid", meter),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )


    def writeInQosRules(
        self, op_type, meter, bytes, interval
    ):
        tbl_global_path = "ig_ctl.ig_ctl_qos_in"
        tbl_name = "%s.policer" % (tbl_global_path)
        self._processMeterFromControlPlane(
            op_type,
            tbl_name,
            meter,
            bytes,
            interval
        )

    def writeOutQosRules(
        self, op_type, meter, bytes, interval
    ):
        tbl_global_path = "ig_ctl.ig_ctl_qos_out"
        tbl_name = "%s.policer" % (tbl_global_path)
        self._processMeterFromControlPlane(
            op_type,
            tbl_name,
            meter,
            bytes,
            interval
        )


    def writeFlowspec4Rules(
        self, op_type, vrf, meter, bytes, interval, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_flowspec"
        tbl_name = "%s.tbl_ipv4_flowspec" % (tbl_global_path)
        tbl_action_name = "%s.act4_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv4.protocol", pr, prm),
            gc.KeyTuple("hdr.ipv4.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv4.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv4.diffserv", ts, tsm),
            gc.KeyTuple("hdr.ipv4.identification", fl, flm),
        ]
        data_field_list = [
             gc.DataTuple("metid", (meter+1)),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        tbl_global_path = "ig_ctl.ig_ctl_flowspec"
        tbl_name = "%s.policer4" % (tbl_global_path)
        self._processMeterFromControlPlane(
            op_type,
            tbl_name,
            (meter+1),
            bytes,
            interval
        )

    def writeFlowspec6Rules(
        self, op_type, vrf, meter, bytes, interval, pri, act, pr, prm, sa, sam, da, dam, sp, spm, dp, dpm, ts, tsm, fl, flm
    ):
        tbl_global_path = "ig_ctl.ig_ctl_flowspec"
        tbl_name = "%s.tbl_ipv6_flowspec" % (tbl_global_path)
        tbl_action_name = "%s.act6_%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("$MATCH_PRIORITY", pri),
            gc.KeyTuple("hdr.ipv6.next_hdr", pr, prm),
            gc.KeyTuple("hdr.ipv6.src_addr", sa, sam),
            gc.KeyTuple("hdr.ipv6.dst_addr", da, dam),
            gc.KeyTuple("ig_md.layer4_srcprt", sp, spm),
            gc.KeyTuple("ig_md.layer4_dstprt", dp, dpm),
            gc.KeyTuple("hdr.ipv6.traffic_class", ts, tsm),
            gc.KeyTuple("hdr.ipv6.flow_label", fl, flm),
        ]
        data_field_list = [
             gc.DataTuple("metid", (meter+1)),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        tbl_global_path = "ig_ctl.ig_ctl_flowspec"
        tbl_name = "%s.policer6" % (tbl_global_path)
        self._processMeterFromControlPlane(
            op_type,
            tbl_name,
            (meter+1),
            bytes,
            interval
        )



    def writeMlocal4rules(
        self, op_type, vrf, sess, dip, sip, ingr, delete2
    ):
        if self.mcast == False:
            return
        if op_type == 1:
            act = "act_local"
        else:
            act = "act_flood"
        tbl_global_path = "ig_ctl.ig_ctl_mcast"
        tbl_name = "%s.tbl_mcast4" % (tbl_global_path)
        tbl_action_name = "%s.%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv4.src_addr", sip),
            gc.KeyTuple("hdr.ipv4.dst_addr", dip),
        ]
        data_field_list = [
             gc.DataTuple("ingr", ingr),
             gc.DataTuple("sess", sess),
        ]
        key_annotation_fields = {
            "hdr.ipv4.src_addr": "ipv4",
            "hdr.ipv4.dst_addr": "ipv4",
        }
        data_annotation_fields = {}
        if delete2 == "add":
            op_type2 = 1
        elif delete2 == "mod":
            op_type2 = 2
        else:
            op_type2 = 3
        self._processEntryFromControlPlane(
            op_type2,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastMgidFromControlPlane(
            op_type2,
            sess,
            self.mcast_nid,
            self.mcast_xid,
        )
        self.mcast_nid = []
        self.mcast_xid = []


    def writeMlocal6rules(
        self, op_type, vrf, sess, dip, sip, ingr, delete2
    ):
        if self.mcast == False:
            return
        if op_type == 1:
            act = "act_local"
        else:
            act = "act_flood"
        tbl_global_path = "ig_ctl.ig_ctl_mcast"
        tbl_name = "%s.tbl_mcast6" % (tbl_global_path)
        tbl_action_name = "%s.%s" % (tbl_global_path, act)
        key_field_list = [
            gc.KeyTuple("ig_md.vrf", vrf),
            gc.KeyTuple("hdr.ipv6.src_addr", sip),
            gc.KeyTuple("hdr.ipv6.dst_addr", dip),
        ]
        data_field_list = [
             gc.DataTuple("ingr", ingr),
             gc.DataTuple("sess", sess),
        ]
        key_annotation_fields = {
            "hdr.ipv6.src_addr": "ipv6",
            "hdr.ipv6.dst_addr": "ipv6",
        }
        data_annotation_fields = {}
        if delete2 == "add":
            op_type2 = 1
        elif delete2 == "mod":
            op_type2 = 2
        else:
            op_type2 = 3
        self._processEntryFromControlPlane(
            op_type2,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastMgidFromControlPlane(
            op_type2,
            sess,
            self.mcast_nid,
            self.mcast_xid,
        )
        self.mcast_nid = []
        self.mcast_xid = []


    def writeMroute4rules(
        self, op_type, vrf, sess, dip, sip, ingr, port, subif, smac, dmac
    ):
        if self.mcast == False:
            return
        nodid = (sess << 16 ) | subif;
        if op_type != 3:
            self.mcast_nid.append(nodid)
            self.mcast_xid.append(0)
        tbl_global_path = "eg_ctl.eg_ctl_mcast"
        tbl_name = "%s.tbl_mcast" % (tbl_global_path)
        tbl_action_name = "%s.act_rawip" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", subif),
        ]
        data_field_list = [
             gc.DataTuple("src_mac_addr", smac),
             gc.DataTuple("dst_mac_addr", dmac),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {"dst_mac_addr": "mac", "src_mac_addr": "mac"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastNodeFromControlPlane(
            op_type,
            nodid,
            subif,
            self.hairpin2recirc(port),
        )



    def writeMroute6rules(
        self, op_type, vrf, sess, dip, sip, ingr, port, subif, smac, dmac
    ):
        if self.mcast == False:
            return
        nodid = (sess << 16 ) | subif;
        if op_type != 3:
            self.mcast_nid.append(nodid)
            self.mcast_xid.append(0)
        tbl_global_path = "eg_ctl.eg_ctl_mcast"
        tbl_name = "%s.tbl_mcast" % (tbl_global_path)
        tbl_action_name = "%s.act_rawip" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", subif),
        ]
        data_field_list = [
             gc.DataTuple("src_mac_addr", smac),
             gc.DataTuple("dst_mac_addr", dmac),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {"dst_mac_addr": "mac", "src_mac_addr": "mac"}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastNodeFromControlPlane(
            op_type,
            nodid,
            subif,
            self.hairpin2recirc(port),
        )


    def writeMlabRouteRules(
        self, op_type, ipver, vrf, sess, dip, sip, ingr, port, hopid, outlab, subif
    ):
        if self.mcast == False:
            return
        nodid = (sess << 16 ) | hopid;
        if op_type != 3:
            self.mcast_nid.append(nodid)
            self.mcast_xid.append(0)

        tbl_global_path = "eg_ctl.eg_ctl_mcast"
        tbl_name = "%s.tbl_mcast" % (tbl_global_path)
        tbl_action_name = "%s.act_encap_ipv%s_mpls" % (tbl_global_path, ipver)
        key_field_list = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", hopid),
        ]
        data_field_list = [
             gc.DataTuple("hop", hopid),
             gc.DataTuple("label", outlab),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastNodeFromControlPlane(
            op_type,
            nodid,
            hopid,
            self.hairpin2recirc(port),
        )


    def writeDupLabelRules(
        self, op_type, vrf, sess, inlab, port, subif, hopid, outlab
    ):
        if self.mcast == False:
            return
        nodid = (sess << 16 ) | hopid;
        if op_type != 3:
            self.mcast_nid.append(nodid)
            self.mcast_xid.append(0)
        tbl_global_path = "eg_ctl.eg_ctl_mcast"
        tbl_name = "%s.tbl_mcast" % (tbl_global_path)
        tbl_action_name = "%s.act_duplab" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", hopid),
        ]
        data_field_list = [
             gc.DataTuple("hop", hopid),
             gc.DataTuple("label", outlab),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastNodeFromControlPlane(
            op_type,
            nodid,
            hopid,
            self.hairpin2recirc(port),
        )


    def writeDupLabLocRules(
        self, op_type, ipver, vrf, sess, inlab, delete2
    ):
        if self.mcast == False:
            return
        if op_type == 1:
            act = "act_decap_mpls_ipv"+ipver
        else:
            act = "act_drop"
        if op_type != 3:
            self.mcast_nid.append(sess << 16)
            self.mcast_xid.append(0)
        tbl_global_path1 = "ig_ctl.ig_ctl_mpls"
        tbl_name1 = "%s.tbl_mpls_fib" % (tbl_global_path1)
        tbl_action_name1 = "%s.act_mpls_bcast_label" % (tbl_global_path1)
        key_field_list1 = [
            gc.KeyTuple("hdr.mpls0.label", inlab),
        ]
        data_field_list1 = [
             gc.DataTuple("sess", sess),
        ]
        key_annotation_fields1 = {}
        data_annotation_fields1 = {}
        if delete2 == "add":
            op_type2 = 1
        elif delete2 == "mod":
            op_type2 = 2
        else:
            op_type2 = 3
        self._processEntryFromControlPlane(
            op_type2,
            tbl_name1,
            key_field_list1,
            data_field_list1,
            tbl_action_name1,
            key_annotation_fields1,
            data_annotation_fields1,
        )

        tbl_global_path2 = "eg_ctl.eg_ctl_mcast"
        tbl_name2 = "%s.tbl_mcast" % (tbl_global_path2)
        tbl_action_name2 = "%s.%s" % (tbl_global_path2, act)
        key_field_list2 = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", 0),
        ]
        data_field_list2 = []
        key_annotation_fields2 = {}
        data_annotation_fields2 = {}
        self._processEntryFromControlPlane(
            op_type2,
            tbl_name2,
            key_field_list2,
            data_field_list2,
            tbl_action_name2,
            key_annotation_fields2,
            data_annotation_fields2,
        )

        self._processMcastNodeFromControlPlane(
            op_type,
            sess << 16,
            0,
            self.recirc_port,
        )

        self._processMcastMgidFromControlPlane(
            op_type2,
            sess,
            self.mcast_nid,
            self.mcast_xid,
        )
        self.mcast_nid = []
        self.mcast_xid = []




    def writeBierLabelRules(
        self, op_type, vrf, sess, inlab, port, subif, hopid, outlab, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7
    ):
        if self.mcast == False:
            return
        nodid = (sess << 16 ) | hopid;
        if op_type != 3:
            self.mcast_nid.append(nodid)
            self.mcast_xid.append(0)
        tbl_global_path = "eg_ctl.eg_ctl_mcast"
        tbl_name = "%s.tbl_mcast" % (tbl_global_path)
        tbl_action_name = "%s.act_bier" % (tbl_global_path)
        key_field_list = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", hopid),
        ]
        data_field_list = [
             gc.DataTuple("hop", hopid),
             gc.DataTuple("label", outlab),
             gc.DataTuple("bs0", bs0 & 0xffffffff),
             gc.DataTuple("bs1", bs1 & 0xffffffff),
             gc.DataTuple("bs2", bs2 & 0xffffffff),
             gc.DataTuple("bs3", bs3 & 0xffffffff),
             gc.DataTuple("bs4", bs4 & 0xffffffff),
             gc.DataTuple("bs5", bs5 & 0xffffffff),
             gc.DataTuple("bs6", bs6 & 0xffffffff),
             gc.DataTuple("bs7", bs7 & 0xffffffff),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastNodeFromControlPlane(
            op_type,
            nodid,
            hopid,
            self.hairpin2recirc(port),
        )


    def writeBierLabLocRules(
        self, op_type, ipver, vrf, sess, inlab, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7
    ):
        if self.mcast == False:
            return
        if op_type != 3:
            self.mcast_nid.append(sess << 16)
            self.mcast_xid.append(0)
        tbl_global_path1 = "ig_ctl.ig_ctl_mpls"
        tbl_name1 = "%s.tbl_mpls_fib" % (tbl_global_path1)
        tbl_action_name1 = "%s.act_mpls_bier_label" % (tbl_global_path1)
        key_field_list1 = [
            gc.KeyTuple("hdr.mpls0.label", inlab),
        ]
        data_field_list1 = [
             gc.DataTuple("sess", sess),
        ]
        key_annotation_fields1 = {}
        data_annotation_fields1 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name1,
            key_field_list1,
            data_field_list1,
            tbl_action_name1,
            key_annotation_fields1,
            data_annotation_fields1,
        )

        tbl_global_path2 = "eg_ctl.eg_ctl_mcast"
        tbl_name2 = "%s.tbl_mcast" % (tbl_global_path2)
        tbl_action_name2 = "%s.act_decap_bier_ipv%s" % (tbl_global_path2, ipver)
        key_field_list2 = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", 0),
        ]
        data_field_list2 = [
             gc.DataTuple("bs0", bs0 & 0xffffffff),
             gc.DataTuple("bs1", bs1 & 0xffffffff),
             gc.DataTuple("bs2", bs2 & 0xffffffff),
             gc.DataTuple("bs3", bs3 & 0xffffffff),
             gc.DataTuple("bs4", bs4 & 0xffffffff),
             gc.DataTuple("bs5", bs5 & 0xffffffff),
             gc.DataTuple("bs6", bs6 & 0xffffffff),
             gc.DataTuple("bs7", bs7 & 0xffffffff),
        ]
        key_annotation_fields2 = {}
        data_annotation_fields2 = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name2,
            key_field_list2,
            data_field_list2,
            tbl_action_name2,
            key_annotation_fields2,
            data_annotation_fields2,
        )

        self._processMcastNodeFromControlPlane(
            op_type,
            sess << 16,
            0,
            self.recirc_port,
        )

        self._processMcastMgidFromControlPlane(
            op_type,
            sess,
            self.mcast_nid,
            self.mcast_xid,
        )
        self.mcast_nid = []
        self.mcast_xid = []


    def writeMbierRouteRules(
        self, op_type, ipver, vrf, sess, dip, sip, ingr, port, hopid, outlab, subif, bfir, si, bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7
    ):
        if self.mcast == False:
            return
        nodid = (sess << 16 ) | (hopid + si);
        if op_type != 3:
            self.mcast_nid.append(nodid)
            self.mcast_xid.append(0)

        tbl_global_path = "eg_ctl.eg_ctl_mcast"
        tbl_name = "%s.tbl_mcast" % (tbl_global_path)
        tbl_action_name = "%s.act_encap_ipv%s_bier" % (tbl_global_path, ipver)
        key_field_list = [
            gc.KeyTuple("hdr.internal.clone_session", sess),
            gc.KeyTuple("eg_intr_md.egress_rid", (hopid + si)),
        ]
        data_field_list = [
             gc.DataTuple("hop", hopid),
             gc.DataTuple("label", outlab),
             gc.DataTuple("bs0", bs0 & 0xffffffff),
             gc.DataTuple("bs1", bs1 & 0xffffffff),
             gc.DataTuple("bs2", bs2 & 0xffffffff),
             gc.DataTuple("bs3", bs3 & 0xffffffff),
             gc.DataTuple("bs4", bs4 & 0xffffffff),
             gc.DataTuple("bs5", bs5 & 0xffffffff),
             gc.DataTuple("bs6", bs6 & 0xffffffff),
             gc.DataTuple("bs7", bs7 & 0xffffffff),
        ]
        key_annotation_fields = {}
        data_annotation_fields = {}
        self._processEntryFromControlPlane(
            op_type,
            tbl_name,
            key_field_list,
            data_field_list,
            tbl_action_name,
            key_annotation_fields,
            data_annotation_fields,
        )
        self._processMcastNodeFromControlPlane(
            op_type,
            nodid,
            hopid,
            self.hairpin2recirc(port),
        )









    def run(self):
        logger.warning("BfForwarder - Main")
        logger.warning("BfForwarder - Entering message loop")
        while not self.die:

            if len(ACTIVE_PORTS.keys())==0:
                logger.debug("BfForwarder - No active ports")
            else:
                logger.debug("BfForwarder - Actie ports %s" % ACTIVE_PORTS.keys())

            # message loop from control plane

            try:
                line = self.file.readline(8192)
            except Exception as e:
                e = sys.exc_info()[0]
                logger.warning("%s - exited with code [%s]" % (self.class_name,_Exception()))
                self.tearDown()

            if len(line) == 0:
                logger.warning("BfForwarder - Empty message from control plane" )
                logger.warning("BfForwarder: connection with control plane lost, quitting ...")
                graceful_exit(bf_client,sck)
                continue

            splt = line.split(" ")
            if not ("portbundle_" in splt[0]):
                logger.warning("rx: %s", splt)
            else:
                continue
            if splt[0] == "route4_add":
                addr = splt[1].split("/")
                self.writeForwardRules4(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
                )
                continue
            if splt[0] == "route4_mod":
                addr = splt[1].split("/")
                self.writeForwardRules4(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
                )
                continue
            if splt[0] == "route4_del":
                addr = splt[1].split("/")
                self.writeForwardRules4(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
                )
                continue

            if splt[0] == "labroute4_add":
                addr = splt[1].split("/")
                self.writeGlobRules4(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
                )
                continue
            if splt[0] == "labroute4_mod":
                addr = splt[1].split("/")
                self.writeGlobRules4(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
                )
                continue
            if splt[0] == "labroute4_del":
                addr = splt[1].split("/")
                self.writeGlobRules4(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
                )
                continue

            if splt[0] == "vpnroute4_add":
                addr = splt[1].split("/")
                self.writeVpnRules4(
                    1,
                    addr[0],
                    int(addr[1]),
                    int(splt[2]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
                continue
            if splt[0] == "vpnroute4_mod":
                addr = splt[1].split("/")
                self.writeVpnRules4(
                    2,
                    addr[0],
                    int(addr[1]),
                    int(splt[2]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
                continue
            if splt[0] == "vpnroute4_del":
                addr = splt[1].split("/")
                self.writeVpnRules4(
                    3,
                    addr[0],
                    int(addr[1]),
                    int(splt[2]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
                continue

            if splt[0] == "srvroute4_add":
                addr = splt[1].split("/")
                self.writeSrvRules4(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "srvroute4_mod":
                addr = splt[1].split("/")
                self.writeSrvRules4(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "srvroute4_del":
                addr = splt[1].split("/")
                self.writeSrvRules4(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue

            if splt[0] == "polroute4_add":
                addr = splt[1].split("/")
                self.writePolkaRules4(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "polroute4_mod":
                addr = splt[1].split("/")
                self.writePolkaRules4(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "polroute4_del":
                addr = splt[1].split("/")
                self.writePolkaRules4(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue

            if splt[0] == "myaddr4_add":
                addr = splt[1].split("/")
                self.writeMyaddrRules4(1, addr[0], int(addr[1]), int(splt[3]))
                continue
            if splt[0] == "myaddr4_mod":
                addr = splt[1].split("/")
                self.writeMyaddrRules4(2, addr[0], int(addr[1]), int(splt[3]))
                continue
            if splt[0] == "myaddr4_del":
                addr = splt[1].split("/")
                self.writeMyaddrRules4(3, addr[0], int(addr[1]), int(splt[3]))
                continue

            if splt[0] == "cpulabel_add":
                self.writeCpuMplsRules(1, int(splt[1]))
                continue
            if splt[0] == "cpulabel_mod":
                self.writeCpuMplsRules(2, int(splt[1]))
                continue
            if splt[0] == "cpulabel_del":
                self.writeCpuMplsRules(3, int(splt[1]))
                continue

            if splt[0] == "label4_add":
                self.writeMplsRules(1, int(splt[1]), int(splt[4]), int(splt[2]))
                continue
            if splt[0] == "label4_mod":
                self.writeMplsRules(2, int(splt[1]), int(splt[4]), int(splt[2]))
                continue
            if splt[0] == "label4_del":
                self.writeMplsRules(3, int(splt[1]), int(splt[4]), int(splt[2]))
                continue

            if splt[0] == "unlabel4_add":
                self.writeUnMplsRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "unlabel4_mod":
                self.writeUnMplsRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "unlabel4_del":
                self.writeUnMplsRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "mylabel4_add":
                self.writeMyMplsRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "mylabel4_mod":
                self.writeMyMplsRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "mylabel4_del":
                self.writeMyMplsRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "mysrv4_add":
                self.writeMySrv4rules(1, int(splt[1]), splt[2], int(splt[3]))
                continue
            if splt[0] == "mysrv4_mod":
                self.writeMySrv4rules(2, int(splt[1]), splt[2], int(splt[3]))
                continue
            if splt[0] == "mysrv4_del":
                self.writeMySrv4rules(3, int(splt[1]), splt[2], int(splt[3]))
                continue

            if splt[0] == "nshfwd_add":
                self.writeNshFwdRules(1,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],int(splt[6]),int(splt[7]))
                continue
            if splt[0] == "nshfwd_mod":
                self.writeNshFwdRules(2,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],int(splt[6]),int(splt[7]))
                continue
            if splt[0] == "nshfwd_del":
                self.writeNshFwdRules(3,int(splt[1]),int(splt[2]),int(splt[3]),splt[4],splt[5],int(splt[6]),int(splt[7]))
                continue

            if splt[0] == "nshloc_add":
                self.writeNshLocRules(1,int(splt[1]),int(splt[2]),int(splt[3]))
                continue
            if splt[0] == "nshloc_mod":
                self.writeNshLocRules(2,int(splt[1]),int(splt[2]),int(splt[3]))
                continue
            if splt[0] == "nshloc_del":
                self.writeNshLocRules(3,int(splt[1]),int(splt[2]),int(splt[3]))
                continue

            if splt[0] == "neigh4_add":
                self.writeNexthopRules(1, int(splt[1]), splt[3], splt[5],int(splt[6]))
                self.writeNeighborRules4(1, splt[2], int(splt[1]), int(splt[4]))
                continue
            if splt[0] == "neigh4_mod":
                self.writeNexthopRules(2, int(splt[1]), splt[3], splt[5],int(splt[6]))
                self.writeNeighborRules4(2, splt[2], int(splt[1]), int(splt[4]))
                continue
            if splt[0] == "neigh4_del":
                self.writeNexthopRules(3, int(splt[1]), splt[3], splt[5],int(splt[6]))
                self.writeNeighborRules4(3, splt[2], int(splt[1]), int(splt[4]))
                continue

            if splt[0] == "hairpin_add":
                self.writeHairpinRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "hairpin_mod":
                self.writeHairpinRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "hairpin_del":
                self.writeHairpinRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "portvrf_add":
                self.writeVrfRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "portvrf_mod":
                self.writeVrfRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "portvrf_del":
                self.writeVrfRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "nhop2port_add":
                self.writeNhop2portRules(1, int(splt[1]), int(splt[2]), int(splt[3]))
                continue
            if splt[0] == "nhop2port_mod":
                self.writeNhop2portRules(2, int(splt[1]), int(splt[2]), int(splt[3]))
                continue
            if splt[0] == "nhop2port_del":
                self.writeNhop2portRules(3, int(splt[1]), int(splt[2]), int(splt[3]))
                continue

            if splt[0] == "portvlan_add":
                self.writeVlanRules(1, int(splt[1]), int(splt[2]), int(splt[3]))
                continue
            if splt[0] == "portvlan_mod":
                self.writeVlanRules(2, int(splt[1]), int(splt[2]), int(splt[3]))
                continue
            if splt[0] == "portvlan_del":
                self.writeVlanRules(3, int(splt[1]), int(splt[2]), int(splt[3]))
                continue

            if splt[0] == "bundlevlan_add":
                self.writeBunVlanRules(1, int(splt[1]), int(splt[2]), int(splt[3]))
                continue
            if splt[0] == "bundlevlan_mod":
                self.writeBunVlanRules(2, int(splt[1]), int(splt[2]), int(splt[3]))
                continue
            if splt[0] == "bundlevlan_del":
                self.writeBunVlanRules(3, int(splt[1]), int(splt[2]), int(splt[3]))
                continue

            if splt[0] == "xconnect_add":
                self.writeXconnRules(
                    1, int(splt[1]), int(splt[3]), int(splt[4]), int(splt[5]), int(splt[6])
                )
                continue
            if splt[0] == "xconnect_mod":
                self.writeXconnRules(
                    2, int(splt[1]), int(splt[3]), int(splt[4]), int(splt[5]), int(splt[6])
                )
                continue
            if splt[0] == "xconnect_del":
                self.writeXconnRules(
                    3, int(splt[1]), int(splt[3]), int(splt[4]), int(splt[5]), int(splt[6])
                )
                continue

            if splt[0] == "portbridge_add":
                self.writeBrprtRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "portbridge_mod":
                self.writeBrprtRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "portbridge_del":
                self.writeBrprtRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "bridgemac_add":
                self.writeBrmacRules(1, int(splt[1]), splt[2], int(splt[3]))
                continue
            if splt[0] == "bridgemac_mod":
                self.writeBrmacRules(2, int(splt[1]), splt[2], int(splt[3]))
                continue
            if splt[0] == "bridgemac_del":
                self.writeBrmacRules(3, int(splt[1]), splt[2], int(splt[3]))
                continue

            if splt[0] == "routedmac_add":
                self.writeRoumacRules(1, int(splt[1]), splt[2], int(splt[3]),int(splt[4]))
                continue
            if splt[0] == "routedmac_mod":
                selfwriteRoumacRules(2, int(splt[1]), splt[2], int(splt[3]),int(splt[4]))
                continue
            if splt[0] == "routedmac_del":
                self.writeRoumacRules(3, int(splt[1]), splt[2], int(splt[3]),int(splt[4]))
                continue

            if splt[0] == "bridgelabel_add":
                self.writeBrlabRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "bridgelabel_mod":
                self.writeBrlabRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "bridgelabel_del":
                self.writeBrlabRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "bridgevpls_add":
                self.writeBrvplsRules(
                    1, int(splt[1]), splt[2], int(splt[4]), int(splt[5]), int(splt[6])
                )
                continue
            if splt[0] == "bridgevpls_mod":
                self.writeBrvplsRules(
                    2, int(splt[1]), splt[2], int(splt[4]), int(splt[5]), int(splt[6])
                )
                continue
            if splt[0] == "bridgevpls_del":
                self.writeBrvplsRules(
                    3, int(splt[1]), splt[2], int(splt[4]), int(splt[5]), int(splt[6])
                )
                continue

            if splt[0] == "route6_add":
                addr = splt[1].split("/")
                self.writeForwardRules6(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
                )
                continue
            if splt[0] == "route6_mod":
                addr = splt[1].split("/")
                self.writeForwardRules6(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
                )
                continue
            if splt[0] == "route6_del":
                addr = splt[1].split("/")
                self.writeForwardRules6(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
                )
                continue

            if splt[0] == "labroute6_add":
                addr = splt[1].split("/")
                self.writeGlobRules6(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
                )
                continue
            if splt[0] == "labroute6_mod":
                addr = splt[1].split("/")
                self.writeGlobRules6(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
                )
                continue
            if splt[0] == "labroute6_del":
                addr = splt[1].split("/")
                self.writeGlobRules6(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
                )
                continue

            if splt[0] == "vpnroute6_add":
                addr = splt[1].split("/")
                self.writeVpnRules6(
                    1,
                    addr[0],
                    int(addr[1]),
                    int(splt[2]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
                continue
            if splt[0] == "vpnroute6_mod":
                addr = splt[1].split("/")
                self.writeVpnRules6(
                    2,
                    addr[0],
                    int(addr[1]),
                    int(splt[2]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
                continue
            if splt[0] == "vpnroute6_del":
                addr = splt[1].split("/")
                self.writeVpnRules6(
                    3,
                    addr[0],
                    int(addr[1]),
                    int(splt[2]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
                continue

            if splt[0] == "srvroute6_add":
                addr = splt[1].split("/")
                self.writeSrvRules6(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "srvroute6_mod":
                addr = splt[1].split("/")
                self.writeSrvRules6(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "srvroute6_del":
                addr = splt[1].split("/")
                self.writeSrvRules6(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue

            if splt[0] == "polroute6_add":
                addr = splt[1].split("/")
                self.writePolkaRules6(
                    1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "polroute6_mod":
                addr = splt[1].split("/")
                self.writePolkaRules6(
                    2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue
            if splt[0] == "polroute6_del":
                addr = splt[1].split("/")
                self.writePolkaRules6(
                    3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
                )
                continue

            if splt[0] == "myaddr6_add":
                addr = splt[1].split("/")
                self.writeMyaddrRules6(1, addr[0], int(addr[1]), int(splt[3]))
                continue
            if splt[0] == "myaddr6_mod":
                addr = splt[1].split("/")
                self.writeMyaddrRules6(2, addr[0], int(addr[1]), int(splt[3]))
                continue
            if splt[0] == "myaddr6_del":
                addr = splt[1].split("/")
                self.writeMyaddrRules6(3, addr[0], int(addr[1]), int(splt[3]))
                continue

            if splt[0] == "label6_add":
                self.writeMplsRules(1, int(splt[1]), int(splt[4]), int(splt[2]))
                continue
            if splt[0] == "label6_mod":
                self.writeMplsRules(2, int(splt[1]), int(splt[4]), int(splt[2]))
                continue
            if splt[0] == "label6_del":
                self.writeMplsRules(3, int(splt[1]), int(splt[4]), int(splt[2]))
                continue

            if splt[0] == "unlabel6_add":
                self.writeUnMplsRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "unlabel6_mod":
                self.writeUnMplsRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "unlabel6_del":
                self.writeUnMplsRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "mylabel6_add":
                self.writeMyMplsRules(1, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "mylabel6_mod":
                self.writeMyMplsRules(2, int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "mylabel6_del":
                self.writeMyMplsRules(3, int(splt[1]), int(splt[2]))
                continue

            if splt[0] == "mysrv6_add":
                self.writeMySrv6rules(1, int(splt[1]), splt[2], int(splt[3]))
                continue
            if splt[0] == "mysrv6_mod":
                self.writeMySrv6rules(2, int(splt[1]), splt[2], int(splt[3]))
                continue
            if splt[0] == "mysrv6_del":
                self.writeMySrv6rules(3, int(splt[1]), splt[2], int(splt[3]))
                continue

            if splt[0] == "neigh6_add":
                self.writeNexthopRules(1,int(splt[1]),splt[3],splt[5],int(splt[6]))
                self.writeNeighborRules6(1, splt[2], int(splt[1]), int(splt[4]))
                continue
            if splt[0] == "neigh6_mod":
                self.writeNexthopRules(2,int(splt[1]),splt[3],splt[5],int(splt[6]))
                self.writeNeighborRules6(2, splt[2], int(splt[1]), int(splt[4]))
                continue
            if splt[0] == "neigh6_del":
                self.writeNexthopRules(3,int(splt[1]),splt[3],splt[5],int(splt[6]))
                self.writeNeighborRules6(3, splt[2], int(splt[1]), int(splt[4]))
                continue

            if splt[0] == "copp4_add":
                self.writeCopp4Rules(
                    1,
                    int(splt[1]),
                    splt[2],
                    int(splt[3]),
                    int(splt[4]),
                    splt[5],
                    splt[6],
                    splt[7],
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                )
                continue
            if splt[0] == "copp4_mod":
                self.writeCopp4Rules(
                    2,
                    int(splt[1]),
                    splt[2],
                    int(splt[3]),
                    int(splt[4]),
                    splt[5],
                    splt[6],
                    splt[7],
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                )
                continue
            if splt[0] == "copp4_del":
                self.writeCopp4Rules(
                    3,
                    int(splt[1]),
                    splt[2],
                    int(splt[3]),
                    int(splt[4]),
                    splt[5],
                    splt[6],
                    splt[7],
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                )
                continue

            if splt[0] == "copp6_add":
                self.writeCopp6Rules(
                    1,
                    int(splt[1]),
                    splt[2],
                    int(splt[3]),
                    int(splt[4]),
                    splt[5],
                    splt[6],
                    splt[7],
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                )
                continue
            if splt[0] == "copp6_mod":
                self.writeCopp6Rules(
                    2,
                    int(splt[1]),
                    splt[2],
                    int(splt[3]),
                    int(splt[4]),
                    splt[5],
                    splt[6],
                    splt[7],
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                )
                continue
            if splt[0] == "copp6_del":
                self.writeCopp6Rules(
                    3,
                    int(splt[1]),
                    splt[2],
                    int(splt[3]),
                    int(splt[4]),
                    splt[5],
                    splt[6],
                    splt[7],
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                )
                continue


            if splt[0] == "natcfg4_add":
                self.writeNatCfgRules4(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "natcfg4_mod":
                self.writeNatCfgRules4(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "natcfg4_del":
                self.writeNatCfgRules4(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue

            if splt[0] == "natcfg6_add":
                self.writeNatCfgRules6(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "natcfg6_mod":
                self.writeNatCfgRules6(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "natcfg6_del":
                self.writeNatCfgRules6(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue



            if splt[0] == "nattrns4_add":
                self.writeNatTrnsRules4(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    splt[5],
                    int(splt[6]),
                    splt[7],
                    int(splt[8]),
                    splt[9],
                    int(splt[10]),
                )
                continue
            if splt[0] == "nattrns4_mod":
                self.writeNatTrnsRules4(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    splt[5],
                    int(splt[6]),
                    splt[7],
                    int(splt[8]),
                    splt[9],
                    int(splt[10]),
                )
                continue
            if splt[0] == "nattrns4_del":
                self.writeNatTrnsRules4(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    splt[5],
                    int(splt[6]),
                    splt[7],
                    int(splt[8]),
                    splt[9],
                    int(splt[10]),
                )
                continue



            if splt[0] == "nattrns6_add":
                self.writeNatTrnsRules6(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    splt[5],
                    int(splt[6]),
                    splt[7],
                    int(splt[8]),
                    splt[9],
                    int(splt[10]),
                )
                continue
            if splt[0] == "nattrns6_mod":
                self.writeNatTrnsRules6(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    splt[5],
                    int(splt[6]),
                    splt[7],
                    int(splt[8]),
                    splt[9],
                    int(splt[10]),
                )
                continue
            if splt[0] == "nattrns6_del":
                self.writeNatTrnsRules6(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    splt[5],
                    int(splt[6]),
                    splt[7],
                    int(splt[8]),
                    splt[9],
                    int(splt[10]),
                )
                continue



            if splt[0] == "pbr4norm_add":
                self.writePbrNormRules4(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr4norm_mod":
                self.writePbrNormRules4(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr4norm_del":
                self.writePbrNormRules4(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue

            if splt[0] == "pbr6norm_add":
                self.writePbrNormRules6(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr6norm_mod":
                self.writePbrNormRules6(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr6norm_del":
                self.writePbrNormRules6(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue

            if splt[0] == "pbr4vrf_add":
                self.writePbrVrfRules4(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr4vrf_mod":
                self.writePbrVrfRules4(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr4vrf_del":
                self.writePbrVrfRules4(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue

            if splt[0] == "pbr6vrf_add":
                self.writePbrVrfRules6(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr6vrf_mod":
                self.writePbrVrfRules6(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr6vrf_del":
                self.writePbrVrfRules6(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue

            if splt[0] == "pbr4hop_add":
                self.writePbrHopRules4(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr4hop_mod":
                self.writePbrHopRules4(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr4hop_del":
                self.writePbrHopRules4(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue

            if splt[0] == "pbr6hop_add":
                self.writePbrHopRules6(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr6hop_mod":
                self.writePbrHopRules6(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "pbr6hop_del":
                self.writePbrHopRules6(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                    splt[10],
                    splt[11],
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue

            if splt[0] == "pbr4lab_add":
                self.writePbrLabRules4(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "pbr4lab_mod":
                self.writePbrLabRules4(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "pbr4lab_del":
                self.writePbrLabRules4(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue

            if splt[0] == "pbr6lab_add":
                self.writePbrLabRules6(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "pbr6lab_mod":
                self.writePbrLabRules6(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "pbr6lab_del":
                self.writePbrLabRules6(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue



            if splt[0] == "gre4_add":
                self.writeGre4rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "gre4_mod":
                self.writeGre4rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "gre4_del":
                self.writeGre4rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue


            if splt[0] == "gre6_add":
                self.writeGre6rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "gre6_mod":
                self.writeGre6rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "gre6_del":
                self.writeGre6rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue


            if splt[0] == "ipip4_add":
                self.writeIpip4rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "ipip4_mod":
                self.writeIpip4rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "ipip4_del":
                self.writeIpip4rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue


            if splt[0] == "ipip6_add":
                self.writeIpip6rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "ipip6_mod":
                self.writeIpip6rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue
            if splt[0] == "ipip6_del":
                self.writeIpip6rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                )
                continue



            if splt[0] == "pppoe_add":
                self.writePppoeRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                )
                continue
            if splt[0] == "pppoe_mod":
                self.writePppoeRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                )
                continue
            if splt[0] == "pppoe_del":
                self.writePppoeRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                )
                continue


            if splt[0] == "l2tp4_add":
                self.writeL2tp4rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "l2tp4_mod":
                self.writeL2tp4rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "l2tp4_del":
                self.writeL2tp4rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue


            if splt[0] == "l2tp6_add":
                self.writeL2tp6rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "l2tp6_mod":
                self.writeL2tp6rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "l2tp6_del":
                self.writeL2tp6rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    splt[5],
                    splt[6],
                    int(splt[7]),
                    splt[8],
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue


            if splt[0] == "bridgevxlan4_add":
                self.writeVxlan4rules(
                    1,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                )
                continue
            if splt[0] == "bridgevxlan4_mod":
                self.writeVxlan4rules(
                    2,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                )
                continue
            if splt[0] == "bridgevxlan4_del":
                self.writeVxlan4rules(
                    3,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                        int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                )
                continue


            if splt[0] == "bridgevxlan6_add":
                self.writeVxlan6rules(
                    1,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                )
                continue
            if splt[0] == "bridgevxlan6_mod":
                self.writeVxlan6rules(
                    2,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                )
                continue
            if splt[0] == "bridgevxlan6_del":
                self.writeVxlan6rules(
                    3,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                )
                continue



            if splt[0] == "bridgepckoudp4_add":
                self.writePckoudp4rules(
                    1,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "bridgepckoudp4_mod":
                self.writePckoudp4rules(
                    2,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "bridgepckoudp4_del":
                self.writePckoudp4rules(
                    3,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue


            if splt[0] == "bridgepckoudp6_add":
                self.writePckoudp6rules(
                    1,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "bridgepckoudp6_mod":
                self.writePckoudp6rules(
                    2,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "bridgepckoudp6_del":
                self.writePckoudp6rules(
                    3,
                    int(splt[1]),
                    splt[2],
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue



            if splt[0] == "inqos_add":
                self.writeInQosRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue
            if splt[0] == "inqos_mod":
                self.writeInQosRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue
            if splt[0] == "inqos_del":
                self.writeInQosRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue
            if splt[0] == "outqos_add":
                self.writeOutQosRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue
            if splt[0] == "outqos_mod":
                self.writeOutQosRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue
            if splt[0] == "outqos_del":
                self.writeOutQosRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue




            if splt[0] == "inqos4_add":
                self.writeInQos4Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "inqos4_mod":
                self.writeInQos4Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "inqos4_del":
                self.writeInQos4Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "outqos4_add":
                self.writeOutQos4Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "outqos4_mod":
                self.writeOutQos4Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "outqos4_del":
                self.writeOutQos4Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "inqos6_add":
                self.writeInQos6Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "inqos6_mod":
                self.writeInQos6Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "inqos6_del":
                self.writeInQos6Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "outqos6_add":
                self.writeOutQos6Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "outqos6_mod":
                self.writeOutQos6Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue
            if splt[0] == "outqos6_del":
                self.writeOutQos6Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    splt[7],
                    splt[8],
                    splt[9],
                    splt[10],
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                )
                continue


            if splt[0] == "flowspec4_add":
                self.writeFlowspec4Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "flowspec4_mod":
                self.writeFlowspec4Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "flowspec4_del":
                self.writeFlowspec4Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "flowspec6_add":
                self.writeFlowspec6Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "flowspec6_mod":
                self.writeFlowspec6Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue
            if splt[0] == "flowspec6_del":
                self.writeFlowspec6Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    int(splt[7]),
                    int(splt[8]),
                    splt[9],
                    splt[10],
                    splt[11],
                    splt[12],
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                    int(splt[20]),
                )
                continue



            if splt[0] == "inacl4_add":
                self.writeInAcl4Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "inacl4_mod":
                self.writeInAcl4Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "inacl4_del":
                self.writeInAcl4Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "outacl4_add":
                self.writeOutAcl4Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "outacl4_mod":
                self.writeOutAcl4Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "outacl4_del":
                self.writeOutAcl4Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "inacl6_add":
                self.writeInAcl6Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "inacl6_mod":
                self.writeInAcl6Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "inacl6_del":
                self.writeInAcl6Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "outacl6_add":
                self.writeOutAcl6Rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "outacl6_mod":
                self.writeOutAcl6Rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue
            if splt[0] == "outacl6_del":
                self.writeOutAcl6Rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    int(splt[4]),
                    int(splt[5]),
                    splt[6],
                    splt[7],
                    splt[8],
                    splt[9],
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                )
                continue


            if splt[0] == "mlocal4_add":
                self.writeMlocal4rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    splt[6],
                )
                continue
            if splt[0] == "mlocal4_mod":
                self.writeMlocal4rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    splt[6],
                )
                continue
            if splt[0] == "mlocal4_del":
                self.writeMlocal4rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    splt[6],
                )
                continue
            if splt[0] == "mlocal6_add":
                self.writeMlocal6rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    splt[6],
                )
                continue
            if splt[0] == "mlocal6_mod":
                self.writeMlocal6rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    splt[6],
                )
                continue
            if splt[0] == "mlocal6_del":
                self.writeMlocal6rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    splt[6],
                )
                continue

            if splt[0] == "mroute4_add":
                self.writeMroute4rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                )
                continue
            if splt[0] == "mroute4_mod":
                self.writeMroute4rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                )
                continue
            if splt[0] == "mroute4_del":
                self.writeMroute4rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                )
                continue
            if splt[0] == "mroute6_add":
                self.writeMroute6rules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                )
                continue
            if splt[0] == "mroute6_mod":
                self.writeMroute6rules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                )
                continue
            if splt[0] == "mroute6_del":
                self.writeMroute6rules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    splt[8],
                    splt[9],
                )
                continue


            if splt[0] == "mlabroute4_add":
                self.writeMlabRouteRules(
                    1,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "mlabroute4_mod":
                self.writeMlabRouteRules(
                    2,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "mlabroute4_del":
                self.writeMlabRouteRules(
                    3,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue

            if splt[0] == "mlabroute6_add":
                self.writeMlabRouteRules(
                    1,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "mlabroute6_mod":
                self.writeMlabRouteRules(
                    2,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue
            if splt[0] == "mlabroute6_del":
                self.writeMlabRouteRules(
                    3,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                )
                continue


            if splt[0] == "duplabloc4_add":
                self.writeDupLabLocRules(
                    1,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                )
                continue
            if splt[0] == "duplabloc4_mod":
                self.writeDupLabLocRules(
                    2,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                )
                continue
            if splt[0] == "duplabloc4_del":
                self.writeDupLabLocRules(
                    3,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                )
                continue
            if splt[0] == "duplabloc6_add":
                self.writeDupLabLocRules(
                    1,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                )
                continue
            if splt[0] == "duplabloc6_mod":
                self.writeDupLabLocRules(
                    2,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                )
                continue
            if splt[0] == "duplabloc6_del":
                self.writeDupLabLocRules(
                    3,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    splt[4],
                )
                continue

            if splt[0] == "duplabel4_add":
                self.writeDupLabelRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                )
                continue
            if splt[0] == "duplabel4_mod":
                self.writeDupLabelRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                )
                continue
            if splt[0] == "duplabel4_del":
                self.writeDupLabelRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                )
                continue
            if splt[0] == "duplabel6_add":
                self.writeDupLabelRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                )
                continue
            if splt[0] == "duplabel6_mod":
                self.writeDupLabelRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                )
                continue
            if splt[0] == "duplabel6_del":
                self.writeDupLabelRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                )
                continue


            if splt[0] == "bierlabel4_add":
                self.writeBierLabelRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                )
                continue
            if splt[0] == "bierlabel4_mod":
                self.writeBierLabelRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                )
                continue
            if splt[0] == "bierlabel4_del":
                self.writeBierLabelRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                )
                continue
            if splt[0] == "bierlabel6_add":
                self.writeBierLabelRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                )
                continue
            if splt[0] == "bierlabel6_mod":
                self.writeBierLabelRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                )
                continue
            if splt[0] == "bierlabel6_del":
                self.writeBierLabelRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                )
                continue

            if splt[0] == "bierlabloc4_add":
                self.writeBierLabLocRules(
                    1,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "bierlabloc4_mod":
                self.writeBierLabLocRules(
                    2,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "bierlabloc4_del":
                self.writeBierLabLocRules(
                    3,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "bierlabloc6_add":
                self.writeBierLabLocRules(
                    1,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "bierlabloc6_mod":
                self.writeBierLabLocRules(
                    2,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue
            if splt[0] == "bierlabloc6_del":
                self.writeBierLabLocRules(
                    3,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                )
                continue

            if splt[0] == "mbierroute4_add":
                self.writeMbierRouteRules(
                    1,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "mbierroute4_mod":
                self.writeMbierRouteRules(
                    2,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "mbierroute4_del":
                self.writeMbierRouteRules(
                    3,
                    "4",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue

            if splt[0] == "mbierroute6_add":
                self.writeMbierRouteRules(
                    1,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "mbierroute6_mod":
                self.writeMbierRouteRules(
                    2,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue
            if splt[0] == "mbierroute6_del":
                self.writeMbierRouteRules(
                    3,
                    "6",
                    int(splt[1]),
                    int(splt[2]),
                    splt[3],
                    splt[4],
                    int(splt[5]),
                    int(splt[6]),
                    int(splt[7]),
                    int(splt[8]),
                    int(splt[9]),
                    int(splt[10]),
                    int(splt[11]),
                    int(splt[12]),
                    int(splt[13]),
                    int(splt[14]),
                    int(splt[15]),
                    int(splt[16]),
                    int(splt[17]),
                    int(splt[18]),
                    int(splt[19]),
                )
                continue


            if splt[0] == "polkapoly_add":
                self.writePolkaPolyRules(
                    1,
                    int(splt[2]),
                )
                continue
            if splt[0] == "polkapoly_mod":
                self.writePolkaPolyRules(
                    2,
                    int(splt[2]),
                )
                continue
            if splt[0] == "polkapoly_del":
                self.writePolkaPolyRules(
                    3,
                    int(splt[2]),
                )
                continue

            if splt[0] == "polkaidx_add":
                self.writePolkaIndexRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue
            if splt[0] == "polkaidx_mod":
                self.writePolkaIndexRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue
            if splt[0] == "polkaidx_del":
                self.writePolkaIndexRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                )
                continue

            if splt[0] == "polkaown_add":
                self.writePolkaOwnRules(
                    1,
                    int(splt[1]),
                    int(splt[2]),
                )
                continue
            if splt[0] == "polkaown_mod":
                self.writePolkaOwnRules(
                    2,
                    int(splt[1]),
                    int(splt[2]),
                )
                continue
            if splt[0] == "polkaown_del":
                self.writePolkaOwnRules(
                    3,
                    int(splt[1]),
                    int(splt[2]),
                )
                continue



            if splt[0] == "mtu":
                self.setPortMTU(int(splt[1]), int(splt[2]))
                continue
            if splt[0] == "state":
                if (self.platform == "wedge100bf32x"):
                    self._setPortAdmStatus(int(splt[1]),int(splt[2]),int(splt[3]),
                                           int(splt[4]),int(splt[5]),int(splt[6]))
                #elif SAL_PORT_ID.has_key(int(splt[1])):
                elif int(splt[1]) in SAL_PORT_ID:
                    self._setPortAdmStatusBF2556X1T(int(splt[1]), int(splt[2]),int(splt[3]),
                                                    int(splt[4]), int(splt[5]),int(splt[6]))
                else:
                    self._setPortAdmStatus(int(splt[1]),int(splt[2]),int(splt[3]),
                                           int(splt[4]),int(splt[5]),int(splt[6]))
                continue
            if splt[0] == "bundlelist_add":
                self.setBundleAdmStatus(1, int(splt[1]), list(splt[2:]))
                continue
            if splt[0] == "bundlelist_mod":
                self.setBundleAdmStatus(2, int(splt[1]), list(splt[2:]))
                continue
            if splt[0] == "bundlelist_del":
                self.setBundleAdmStatus(3, int(splt[1]), list(splt[2:]))
                continue

def str2bool(v):
    if isinstance(v, bool):
       return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

def is_any_thread_alive(threads):
    return True in [t.is_alive() for t in threads]

def graceful_exit(bf_client,sck):
    os._exit(0)
    bf_client.interface._tear_down_stream()
    sck.close()
    sys.exit(0)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="BfRuntime controller")

    parser.add_argument(
        "--bfruntime-address",
        help="BfRuntime address",
        type=str,
        action="store",
        required=False,
        default="127.0.0.1:50052",
    )
    parser.add_argument(
        "--freerouter-address",
        help="freerouter address",
        type=str,
        action="store",
        required=False,
        default="127.0.0.1",
    )
    parser.add_argument(
        "--freerouter-port",
        help="freerouter port",
        type=int,
        action="store",
        required=False,
        default=9080,
    )
    parser.add_argument(
        "--p4-program-name",
        help="run bf_forwarder.py agaisnt p4 program",
        type=str,
        action="store",
        required=False,
        default="bf_router",
    )
    parser.add_argument(
        "--client-id",
        help="bf_forwarder.py gprc client-id",
        type=int,
        action="store",
        required=False,
        default=0,
    )
    parser.add_argument(
        "--pipe-name",
        help="bf_forwarder.py grpc pipe-name",
        type=str,
        action="store",
        required=False,
        default="pipe",
    )
    parser.add_argument(
        "--brdg",
        help="enable bridge",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--mpls",
        help="enable mpls",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--srv6",
        help="enable srv6",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--polka",
        help="enable polka",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--nsh",
        help="enable nsh",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--nat",
        help="enable nat",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--pbr",
        help="enable pbr",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--tun",
        help="enable tunnel",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--poe",
        help="enable pppoe",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--mcast",
        help="enable multicast",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--snmp",
        help="enable snmp export locally",
        type=str2bool,
        nargs='?',
        const=True,
        action="store",
        required=False,
        default=False,
    )
    parser.add_argument(
        "--ifmibs-dir",
        help="Path to the directory where the interface MIBs are stored",
        type=str,
        action="store",
        required=False,
        default="/var/run/bf_router",
    )
    parser.add_argument(
        "--stats-interval",
        help="Interval in seconds between updates of the MIB objects",
        type=int,
        action="store",
        required=False,
        default=5,
    )
    parser.add_argument(
        "--ifindex",
        help="Path to the ifindex MIB file definition",
        type=str,
        action="store",
        required=False,
        default="/root/rare/snmp/ifindex",
    )
    parser.add_argument(
        "--platform",
        help="Platform used: wedge100bf32x, bf2556x1t ",
        type=str,
        action="store",
        required=False,
        default="wedge100bf32x",
    )
    parser.add_argument(
        "--sal-grpc-server-address",
        help="SAL GRPC server address",
        type=str,
        action="store",
        required=False,
        default="127.0.0.1:50054",
    )
    args = parser.parse_args()

    try:
        ACTIVE_PORTS = {}
        PORTS_OPER_STATUS = {}
        ACTIVE_SUBIFS = {}
        SUBIF_COUNTERS = {}
        SUBIF_COUNTERS_IN = {}
        SUBIF_COUNTERS_OUT = {}
        if (args.platform=="bf2556x1t"):
            # start TOFINO via SAL GRPC client
            logger.warning('Starting TOFINO via SAL')
            sal_client = SalGrpcClient(args.sal_grpc_server_address)
            sal_client.TestConnection()
            sal_client.GetSwitchModel()
            sal_client.StartTofino()
            sal_client.StartGearBox()
        else:
            sal_client = None

        logger.warning("%s running on: %s" % (PROGRAM_NAME,args.platform.upper()))

        sck = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sck.connect((args.freerouter_address,
                     args.freerouter_port))
        sckrw_file = sck.makefile("rw")
        sckw_file = sck.makefile("w")


        bf_client = BfRuntimeGrpcClient(args.bfruntime_address,
                                    args.p4_program_name,
                                    args.client_id,
                                    args.pipe_name,)

        if args.snmp:
            bf_snmp = BfIfSnmpClient(1,
                             "bf_snmp",
                             bf_client,
                             args.ifmibs_dir,
                             args.stats_interval,
                             args.ifindex)
            bf_snmp.daemon=True
            bf_snmp.start()
            logger.warning("bf_switchd started with SNMP export")
        else:
            logger.warning("bf_switchd started with no SNMP export")

        bf_forwarder = BfForwarder(2,
                               "bf_forwarder",
                               args.platform,
                               bf_client,
                               sal_client,
                               sckrw_file,
                               args.brdg,
                               args.mpls,
                               args.srv6,
                               args.nat,
                               args.pbr,
                               args.tun,
                               args.poe,
                               args.mcast,
                               args.polka,
                               args.nsh,
                               )

        bf_forwarder.daemon=True
        bf_forwarder.start()

        bf_if_status = BfIfStatus(3,
                                "bf_if_status",
                                bf_client,
                                sckw_file,
                                1)

        bf_if_status.daemon=True
        bf_if_status.start()

        bf_if_counter = BfSubIfCounter(4,
                                "bf_if_counter",
                                bf_client,
                                sckw_file,
                                args.pipe_name,
                                5)

        bf_if_counter.daemon=True
        bf_if_counter.start()

        if args.snmp:
            ALL_THREADS = [bf_snmp,bf_forwarder,bf_if_status,bf_if_counter]
        else:
            ALL_THREADS = [bf_forwarder,bf_if_status,bf_if_counter]

        while is_any_thread_alive(ALL_THREADS):
            [t.join(1) for t in ALL_THREADS
                         if t is not None and t.is_alive()]

    except socket.error as e:
        logger.error("\n%s - control plane not detected ..." % PROGRAM_NAME)
        logger.error("%s - Is control plane started ?" % PROGRAM_NAME)
        sys.exit(1)
    except KeyboardInterrupt:
        logger.warning("\n%s - Received signal 2 ..." % PROGRAM_NAME)
        for t in ALL_THREADS:
            t.die = True
        logger.warning("%s - Quitting !" % PROGRAM_NAME)
        graceful_exit(bf_client,sck)
